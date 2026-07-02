#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>

#define GGPSY_RAD_TO_DEG 57.2957795130823208768

/* Keep text upright in the same user-facing sense as geomtextpath. */
static double ggpsy_text_angle(double angle, int upright)
{
    while (angle <= -180.0) angle += 360.0;
    while (angle > 180.0) angle -= 360.0;
    if (upright) {
        if (angle > 90.0) angle -= 180.0;
        if (angle < -90.0) angle += 180.0;
    }
    return angle;
}

/* When upright handling would flip a label, keep the original path side instead
 * so labels match the historical visual orientation of ggpsychro path labels.
 */
static int ggpsy_text_flipped(double angle, int upright)
{
    double degrees = angle * GGPSY_RAD_TO_DEG;
    while (degrees <= -180.0) degrees += 360.0;
    while (degrees > 180.0) degrees -= 360.0;
    return upright && (degrees > 90.0 || degrees < -90.0);
}

/* Interpolate a point and local tangent on an arclength-parametrised path. */
static void ggpsy_path_at(double target, const double *x, const double *y,
                          const double *arc, int n, double *x_out,
                          double *y_out, double *angle_out)
{
    int i;
    double length = arc[n - 1];

    if (target <= 0.0) {
        i = 0;
        target = 0.0;
    } else if (target >= length) {
        i = n - 2;
        target = length;
    } else {
        /* Interpolation happens for every rendered piece. Binary search keeps
         * long contour paths cheap without changing placement semantics.
         */
        int left = 0;
        int right = n - 1;
        while (right - left > 1) {
            int mid = left + (right - left) / 2;
            if (arc[mid] < target) {
                left = mid;
            } else {
                right = mid;
            }
        }
        i = left;
    }

    double seg = arc[i + 1] - arc[i];
    double frac = seg > 0.0 ? (target - arc[i]) / seg : 0.0;
    double dx = x[i + 1] - x[i];
    double dy = y[i + 1] - y[i];

    *x_out = x[i] + frac * dx;
    *y_out = y[i] + frac * dy;
    *angle_out = atan2(dy, dx);
}

/* Copy one label path, dropping repeated or invalid points before arclength work. */
static int ggpsy_clean_path(const double *x, const double *y,
                            const int *id, int n_path, int label,
                            double *px, double *py, double *arc)
{
    int n = 0;
    for (int i = 0; i < n_path; i++) {
        if (id[i] != label || !R_FINITE(x[i]) || !R_FINITE(y[i])) {
            continue;
        }
        if (n > 0 && x[i] == px[n - 1] && y[i] == py[n - 1]) {
            continue;
        }
        px[n] = x[i];
        py[n] = y[i];
        n++;
    }

    if (n < 2) return 0;

    arc[0] = 0.0;
    for (int i = 1; i < n; i++) {
        double dx = px[i] - px[i - 1];
        double dy = py[i] - py[i - 1];
        arc[i] = arc[i - 1] + hypot(dx, dy);
    }
    return arc[n - 1] > 0.0 ? n : 0;
}

/* Count placed pieces first so the returned R vectors can be allocated exactly. */
static int ggpsy_count_pieces(const double *x, const double *y,
                              const int *id, int n_path,
                              const int *piece_id, int n_piece,
                              const double *piece_mid,
                              const double *piece_width,
                              const double *label_width,
                              const double *label_offset,
                              const double *hjust,
                              int n_label, int remove_long)
{
    double *px = (double *) R_alloc(n_path, sizeof(double));
    double *py = (double *) R_alloc(n_path, sizeof(double));
    double *arc = (double *) R_alloc(n_path, sizeof(double));
    int count = 0;

    for (int label = 1; label <= n_label; label++) {
        int n = ggpsy_clean_path(x, y, id, n_path, label, px, py, arc);
        double width = label_width[label - 1];
        double offset = label_offset[label - 1];
        double hj = hjust[label - 1];
        if (n < 2 || !R_FINITE(width) || !R_FINITE(offset) || !R_FINITE(hj)) {
            continue;
        }
        if (remove_long && width > arc[n - 1]) continue;

        for (int piece = 0; piece < n_piece; piece++) {
            if (piece_id[piece] == label &&
                    R_FINITE(piece_mid[piece]) &&
                    R_FINITE(piece_width[piece])) {
                count++;
            }
        }
    }
    return count;
}

/* Place grid-label glyphs or whole expression labels along their panel paths. */
SEXP C_psychro_textpath_place(SEXP x_sxp, SEXP y_sxp, SEXP id_sxp,
                              SEXP piece_id_sxp, SEXP piece_mid_sxp,
                              SEXP piece_width_sxp,
                              SEXP label_width_sxp,
                              SEXP label_offset_sxp, SEXP hjust_sxp,
                              SEXP upright_sxp, SEXP remove_long_sxp,
                              SEXP keep_path_side_sxp)
{
    if (!isReal(x_sxp) || !isReal(y_sxp) || !isInteger(id_sxp) ||
        !isInteger(piece_id_sxp) || !isReal(piece_mid_sxp) ||
        !isReal(piece_width_sxp) || !isReal(label_width_sxp) ||
        !isReal(label_offset_sxp) || !isReal(hjust_sxp)) {
        error("Invalid grid label placement input.");
    }

    int n_path = LENGTH(x_sxp);
    int n_piece = LENGTH(piece_id_sxp);
    int n_label = LENGTH(label_width_sxp);
    if (LENGTH(y_sxp) != n_path || LENGTH(id_sxp) != n_path ||
        LENGTH(piece_mid_sxp) != n_piece ||
        LENGTH(piece_width_sxp) != n_piece ||
        LENGTH(label_offset_sxp) != n_label ||
        LENGTH(hjust_sxp) != n_label) {
        error("Inconsistent grid label placement input lengths.");
    }

    const double *x = REAL(x_sxp);
    const double *y = REAL(y_sxp);
    const int *id = INTEGER(id_sxp);
    const int *piece_id = INTEGER(piece_id_sxp);
    const double *piece_mid = REAL(piece_mid_sxp);
    const double *piece_width = REAL(piece_width_sxp);
    const double *label_width = REAL(label_width_sxp);
    const double *label_offset = REAL(label_offset_sxp);
    const double *hjust = REAL(hjust_sxp);
    int upright = asLogical(upright_sxp) == TRUE;
    int remove_long = asLogical(remove_long_sxp) == TRUE;
    int keep_path_side = asLogical(keep_path_side_sxp) == TRUE;

    int n_out = ggpsy_count_pieces(
        x, y, id, n_path, piece_id, n_piece, piece_mid, piece_width,
        label_width, label_offset, hjust, n_label, remove_long
    );

    SEXP x_out_sxp = PROTECT(allocVector(REALSXP, n_out));
    SEXP y_out_sxp = PROTECT(allocVector(REALSXP, n_out));
    SEXP angle_out_sxp = PROTECT(allocVector(REALSXP, n_out));
    SEXP piece_out_sxp = PROTECT(allocVector(INTSXP, n_out));
    SEXP label_out_sxp = PROTECT(allocVector(INTSXP, n_out));
    SEXP left_out_sxp = PROTECT(allocVector(REALSXP, n_out));
    SEXP right_out_sxp = PROTECT(allocVector(REALSXP, n_out));
    double *x_out = REAL(x_out_sxp);
    double *y_out = REAL(y_out_sxp);
    double *angle_out = REAL(angle_out_sxp);
    int *piece_out = INTEGER(piece_out_sxp);
    int *label_out = INTEGER(label_out_sxp);
    double *left_out = REAL(left_out_sxp);
    double *right_out = REAL(right_out_sxp);

    double *px = (double *) R_alloc(n_path, sizeof(double));
    double *py = (double *) R_alloc(n_path, sizeof(double));
    double *arc = (double *) R_alloc(n_path, sizeof(double));
    int out = 0;

    for (int label = 1; label <= n_label; label++) {
        int n = ggpsy_clean_path(x, y, id, n_path, label, px, py, arc);
        double width = label_width[label - 1];
        double offset = label_offset[label - 1];
        double hj = hjust[label - 1];
        if (n < 2 || !R_FINITE(width) || !R_FINITE(offset) || !R_FINITE(hj)) {
            continue;
        }
        double length = arc[n - 1];
        if (remove_long && width > length) continue;

        /* Numeric hjust follows text hjust on the available path interval:
         * 0 places the label at the start, 1 at the end, and .5 centres it.
         */
        double start = hj * (length - width);
        double end = start + width;
        double center_x, center_y, center_theta;
        ggpsy_path_at(
            start + 0.5 * width, px, py, arc, n,
            &center_x, &center_y, &center_theta
        );
        int needs_upright_flip = ggpsy_text_flipped(center_theta, upright);
        int use_path_side = keep_path_side && needs_upright_flip;
        int flip_label = !keep_path_side && needs_upright_flip;

        for (int piece = 0; piece < n_piece; piece++) {
            if (piece_id[piece] != label) continue;
            if (!R_FINITE(piece_mid[piece]) || !R_FINITE(piece_width[piece])) {
                continue;
            }

            /* When upright text would require a 180-degree correction, place
             * pieces in mirrored arclength order as well as rotating them.
             * Otherwise each glyph is upright but the word reads backwards.
             */
            double mid = flip_label ? width - piece_mid[piece] : piece_mid[piece];
            double target = start + mid;
            double x_mid, y_mid, theta;
            double x_left, y_left, theta_left;
            double x_right, y_right, theta_right;
            double half_width = 0.5 * piece_width[piece];

            ggpsy_path_at(target, px, py, arc, n, &x_mid, &y_mid, &theta);
            ggpsy_path_at(
                target - half_width, px, py, arc, n,
                &x_left, &y_left, &theta_left
            );
            ggpsy_path_at(
                target + half_width, px, py, arc, n,
                &x_right, &y_right, &theta_right
            );

            double dx = x_right - x_left;
            double dy = y_right - y_left;
            if (dx != 0.0 || dy != 0.0) {
                theta = atan2(dy, dx);
            }
            if (flip_label) {
                theta += M_PI;
            }

            x_out[out] = x_mid - sin(theta) * offset;
            y_out[out] = y_mid + cos(theta) * offset;
            /* Labels in this orientation already read correctly on the raw
             * path side. Avoid rotating them to the opposite historical visual
             * orientation.
             */
            angle_out[out] = ggpsy_text_angle(
                theta * GGPSY_RAD_TO_DEG, (use_path_side || flip_label) ? 0 : upright
            );
            piece_out[out] = piece + 1;
            label_out[out] = label;
            left_out[out] = start;
            right_out[out] = end;
            out++;
        }
    }

    SEXP out_sxp = PROTECT(allocVector(VECSXP, 7));
    SET_VECTOR_ELT(out_sxp, 0, x_out_sxp);
    SET_VECTOR_ELT(out_sxp, 1, y_out_sxp);
    SET_VECTOR_ELT(out_sxp, 2, angle_out_sxp);
    SET_VECTOR_ELT(out_sxp, 3, piece_out_sxp);
    SET_VECTOR_ELT(out_sxp, 4, label_out_sxp);
    SET_VECTOR_ELT(out_sxp, 5, left_out_sxp);
    SET_VECTOR_ELT(out_sxp, 6, right_out_sxp);

    SEXP names_sxp = PROTECT(allocVector(STRSXP, 7));
    SET_STRING_ELT(names_sxp, 0, mkChar("x"));
    SET_STRING_ELT(names_sxp, 1, mkChar("y"));
    SET_STRING_ELT(names_sxp, 2, mkChar("angle"));
    SET_STRING_ELT(names_sxp, 3, mkChar("piece"));
    SET_STRING_ELT(names_sxp, 4, mkChar("label"));
    SET_STRING_ELT(names_sxp, 5, mkChar("left"));
    SET_STRING_ELT(names_sxp, 6, mkChar("right"));
    setAttrib(out_sxp, R_NamesSymbol, names_sxp);

    UNPROTECT(9);
    return out_sxp;
}
