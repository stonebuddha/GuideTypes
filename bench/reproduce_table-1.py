#!/usr/bin/env python3

from tabulate import tabulate
import subprocess

TOOL = "gtypes"
LOG = "table-1.log"

BENCHS = [
    ("lr", "anglican/linear-regression"),
    ("gmm", "anglican/gmm"),
    ("kalman", "anglican/kalman"),
    ("sprinkler", "anglican/sprinkler-bayes-net"),
    ("hmm", "anglican/hmm"),
    ("branching", "anglican/branching"),
    ("marsaglia", "anglican/marsaglia-normal"),
    ("ptrace", "anglican/poisson-trace"),
    ("aircraft", "anglican/aircraft"),
    ("weight", "pyro/weight-model"),
    ("vae", "pyro/vae-model"),
    ("ex-1", "tests/example-1-model"),
    ("ex-2", "tests/example-2"),
    ("gp-dsl", "tests/time-series-model"),
]


def look_for_runtime(msg):
    lines = msg.splitlines()
    for line in lines:
        if line.find("total time") != -1:
            return line.split(":")[-1].strip()
    return "N/A"


def execute(out, task):
    name, path = task
    loc = "N/A"
    with open(path, "r") as f:
        loc = str(len(f.readlines()))
    cmd = [TOOL, "type-check", path]
    ret = subprocess.run(cmd, stdout=subprocess.PIPE)
    msg = str(ret.stdout, "utf-8")

    out.write("Benchmark %s\n" % name)
    out.write("-" * 80 + "\n")
    out.write(msg)
    out.write("=" * 80 + "\n\n\n")

    return (name, path, loc, look_for_runtime(msg))


if __name__ == "__main__":
    with open(LOG, "w") as f:
        tab = []
        for bench in BENCHS:
            tab.append(execute(f, bench))
        print(tabulate(tab, headers=["Name", "Path", "LOC", "Check Time"]))
