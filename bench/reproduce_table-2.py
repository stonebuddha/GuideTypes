#!/usr/bin/env python3

import subprocess
from tabulate import tabulate

TOOL = "gtypes"
LOG = "table-2.log"

BENCHS = [
    ("ex_1", "tests/example-1-model", "tests/example-1-proposal", "IS"),
    ("branching", "anglican/branching", "anglican/branching-proposal", "IS"),
    ("gmm", "anglican/gmm", "anglican/gmm-proposal", "IS"),

    ("weight", "pyro/weight-model", "pyro/weight-guide", "VI"),
    ("vae", "pyro/vae-model", "pyro/vae-guide", "VI"),
]


def look_for_runtime(msg):
    lines = msg.splitlines()
    for line in lines:
        if line.find("total time") != -1:
            text = line.split(":")[-1].strip()
            if text[-2:] == "ns":
                return float(text[:-2]) / 1e6
            elif text[-2:] == "us":
                return float(text[:-2]) / 1e3
            elif text[-2:] == 'ms':
                return float(text[:-2])
            elif text[-1:] == 's':
                return float(text[:-1]) * 1e3
    return None


def run_task(out, bench):
    (name, model, guide, infer) = bench
    print("Benchmark %s: testing ..." % name)
    out.write("Benchmark %s\n" % name)
    out.write("-" * 80 + "\n")

    cmd1 = [TOOL, "compile-m", model]
    ret1 = subprocess.run(cmd1, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    cmd2 = (TOOL, "compile-g", "-m", model, "-g", guide)
    ret2 = subprocess.run(cmd2, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    cde1 = str(ret1.stderr, "utf-8").splitlines()
    cde2 = str(ret2.stderr, "utf-8").splitlines()
    cde = []
    cde.extend([l+"\n" for l in cde1])
    cde.extend([l+"\n" for l in cde2])
    loc = len(cde)

    ctime = look_for_runtime(str(ret1.stdout, "utf-8")) + \
        look_for_runtime(str(ret2.stdout, "utf-8"))
    out.write(str(ret1.stdout, "utf-8") + "\n\n" +
              str(ret2.stdout, "utf-8") + "\n\n")

    hw_file = "%s_handwritten.py" % name
    hw_loc = "N/A"
    hw_cde = []
    with open(hw_file, "r") as f:
        hw_cde = f.readlines()
        hw_loc = str(len(hw_cde))

    subprocess.run(["cp", "%s_test.py.example" % name, "%s_test.py" % name])
    tmp = []
    with open("%s_test.py" % name, "r") as f:
        tmp = f.readlines()

    tmp_new = []
    for line in tmp:
        line_s = line.strip()
        if line_s == "{{ GENERATED }}":
            tmp_new.extend(cde)
        elif line_s == "{{ HANDWRITTEN }}":
            tmp_new.extend(hw_cde)
        else:
            tmp_new.append(line)

    with open("%s_test.py" % name, "w") as f:
        f.writelines(tmp_new)

    ret = subprocess.run(["./%s_test.py" % name], stdout=subprocess.PIPE)
    msg = str(ret.stdout, "utf-8").splitlines()
    gi = msg[0].split(":")[-1].strip()
    hi = msg[1].split(":")[-1].strip()

    out.write("GI: %s, HI: %s\n\n\n\n\n\n" % (gi, hi))

    return (name, infer, ctime, loc, gi, hw_loc, hi)


if __name__ == "__main__":
    with open(LOG, "w") as f:
        data = []
        for bench in BENCHS:
            data.append(run_task(f, bench))
        print(tabulate(data, headers=[
              "Name", "BI", "CG (ms)", "GLOC", "GI (s)", "HLOC", "HI (s)"]))
