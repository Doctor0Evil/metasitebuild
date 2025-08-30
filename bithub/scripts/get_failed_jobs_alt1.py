#!/usr/bin/env python3
"""
Primary script: queries Bit.Hub API or local logs for failed jobs.
Outputs JSON array to stdout.
"""
import json, sys, os

# Example: read from a local log file
log_path = os.environ.get("BIT_HUB_LOG", "bithub/logs/jobs.log")
failed = []

if os.path.exists(log_path):
    with open(log_path) as f:
        for line in f:
            if "FAILED" in line.upper():
                failed.append({"job": line.strip()})
else:
    print(f"::warning::Log file {log_path} not found, returning empty list", file=sys.stderr)

json.dump(failed, sys.stdout)
