#!/usr/bin/env python3
"""Regenerate comfort model oracle values from pythermalcomfort."""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

import numpy as np
import pythermalcomfort
from pythermalcomfort.models import (
    adaptive_ashrae,
    adaptive_en,
    pmv_ppd_iso,
    set_tmp,
)


ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "tests" / "testthat" / "fixtures" / "comfort-oracle.csv"


# Scalar conversion keeps the CSV stable across numpy and plain-Python returns.
def scalar(value: Any) -> Any:
    """Convert numpy scalar values into plain Python values for CSV output."""
    if isinstance(value, np.generic):
        value = value.item()
    return value


# Model outputs may be scalar or vectorized; store both as indexed long rows.
def iter_values(value: Any) -> list[Any]:
    """Return one or more scalar values from a pythermalcomfort field."""
    if isinstance(value, np.ndarray):
        values = value.tolist()
        return values if isinstance(values, list) else [values]
    if isinstance(value, (list, tuple)):
        return list(value)
    return [value]


# Type tags let the R tests reconstruct values without guessing from strings.
def value_type(value: Any) -> str:
    """Return the CSV type tag used by the R test helper."""
    value = scalar(value)
    if isinstance(value, bool):
        return "logical"
    if isinstance(value, str):
        return "character"
    return "numeric"


# Values are written as text so git diffs stay simple and reviewable.
def value_text(value: Any) -> str:
    """Serialize one oracle value without losing numeric precision."""
    value = scalar(value)
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, str):
        return value
    if value is None or not math.isfinite(float(value)):
        return "NA"
    return format(float(value), ".17g")


# Each pythermalcomfort return field becomes one or more indexed oracle rows.
def add_rows(rows: list[dict[str, str]], model: str, case: str, out: Any) -> None:
    """Append all public return fields for one model/case call."""
    for metric, value in out.__dict__.items():
        for index, item in enumerate(iter_values(value), start=1):
            rows.append(
                {
                    "model": model,
                    "case": case,
                    "metric": metric,
                    "index": str(index),
                    "type": value_type(item),
                    "value": value_text(item),
                }
            )


# The script intentionally records representative edge cases instead of a full
# Cartesian product, keeping the fixture small enough to audit in code review.
def main() -> None:
    rows: list[dict[str, str]] = [
        {
            "model": "meta",
            "case": "pythermalcomfort",
            "metric": "version",
            "index": "1",
            "type": "character",
            "value": pythermalcomfort.__version__,
        }
    ]

    pmv_cases = {
        "si_default_22": dict(tdb=22, tr=25, vr=0.1, rh=50, met=1.4, clo=0.5),
        "si_default_25": dict(tdb=25, tr=25, vr=0.1, rh=50, met=1.4, clo=0.5),
        "vector_mixed_inputs": dict(
            tdb=[22, 25, 28],
            tr=[25, 25, 26],
            vr=[0.1, 0.2, 0.3],
            rh=[50, 55, 60],
            met=[1.1, 1.4, 1.8],
            clo=[0.5, 0.6, 0.7],
        ),
        "si_unrounded": dict(
            tdb=26,
            tr=25,
            vr=0.2,
            rh=60,
            met=1.2,
            clo=0.7,
            round_output=False,
        ),
        "ip_default": dict(
            tdb=77,
            tr=77,
            vr=0.328084,
            rh=50,
            met=1.2,
            clo=0.5,
            units="IP",
        ),
        "limit_inputs_low_tdb": dict(
            tdb=5, tr=25, vr=0.1, rh=50, met=1.2, clo=0.5
        ),
        "limit_inputs_false_low_tdb": dict(
            tdb=5,
            tr=25,
            vr=0.1,
            rh=50,
            met=1.2,
            clo=0.5,
            limit_inputs=False,
            round_output=False,
        ),
        "high_rh_clo": dict(tdb=24, tr=24, vr=0.1, rh=100, met=1.2, clo=1.5),
        "upper_tr_v_met": dict(tdb=28, tr=35, vr=1, rh=70, met=2, clo=0.3),
    }
    for case, args in pmv_cases.items():
        add_rows(rows, "pmv", case, pmv_ppd_iso(**args))

    set_cases = {
        "gagge_default": dict(tdb=25, tr=25, v=0.1, rh=50, met=1.2, clo=0.5),
        "vector_mixed_inputs": dict(
            tdb=[24, 26, 28],
            tr=[24, 27, 30],
            v=[0.1, 0.4, 0.8],
            rh=[45, 60, 70],
            met=[1.1, 1.6, 2.0],
            clo=[0.5, 0.7, 0.8],
        ),
        "standing_unrounded": dict(
            tdb=28,
            tr=30,
            v=0.6,
            rh=70,
            met=1.6,
            clo=0.7,
            wme=0.1,
            round_output=False,
        ),
        "sitting_low_pressure": dict(
            tdb=25,
            tr=26,
            v=0.2,
            rh=45,
            met=1.1,
            clo=0.6,
            body_surface_area=1.7,
            p_atm=90000,
            position="sitting",
        ),
        "limit_inputs_low_tdb": dict(
            tdb=5, tr=25, v=0.1, rh=50, met=1.2, clo=0.5
        ),
        "limit_inputs_false_low_tdb": dict(
            tdb=5,
            tr=25,
            v=0.1,
            rh=50,
            met=1.2,
            clo=0.5,
            limit_inputs=False,
            round_output=False,
        ),
        "boundary_low": dict(tdb=10, tr=10, v=0, rh=0, met=1, clo=0),
        "boundary_high": dict(tdb=35, tr=35, v=2, rh=100, met=4, clo=1.5),
        "wme_unrounded": dict(
            tdb=26,
            tr=27,
            v=0.4,
            rh=55,
            met=2,
            clo=0.6,
            wme=0.4,
            round_output=False,
        ),
    }
    for case, args in set_cases.items():
        add_rows(rows, "set", case, set_tmp(**args))

    adaptive_ashrae_cases = {
        "default": dict(tdb=25, tr=25, t_running_mean=20, v=0.1),
        "vector_mixed_inputs": dict(
            tdb=[24, 27, 30],
            tr=[24, 28, 30],
            t_running_mean=[18, 24, 30],
            v=[0.1, 1.0, 1.3],
        ),
        "high_air_speed": dict(tdb=27, tr=27, t_running_mean=24, v=1.0),
        "ip_default": dict(
            tdb=77, tr=77, t_running_mean=68, v=0.328084, units="IP"
        ),
        "limit_inputs_false_low_running": dict(
            tdb=25,
            tr=25,
            t_running_mean=5,
            v=0.1,
            limit_inputs=False,
            round_output=False,
        ),
    }
    for case, args in adaptive_ashrae_cases.items():
        add_rows(rows, "adaptive_ashrae", case, adaptive_ashrae(**args))

    adaptive_en_cases = {
        "default": dict(tdb=25, tr=25, t_running_mean=20, v=0.1),
        "vector_mixed_inputs": dict(
            tdb=[24, 27, 30],
            tr=[24, 28, 30],
            t_running_mean=[18, 24, 30],
            v=[0.1, 1.0, 1.3],
        ),
        "high_air_speed": dict(tdb=27, tr=27, t_running_mean=24, v=1.0),
        "ip_default": dict(
            tdb=77, tr=77, t_running_mean=68, v=0.328084, units="IP"
        ),
        "limit_inputs_false_low_running": dict(
            tdb=25,
            tr=25,
            t_running_mean=5,
            v=0.1,
            limit_inputs=False,
            round_output=False,
        ),
    }
    for case, args in adaptive_en_cases.items():
        add_rows(rows, "adaptive_en", case, adaptive_en(**args))

    with OUT.open("w", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=["model", "case", "metric", "index", "type", "value"],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    main()
