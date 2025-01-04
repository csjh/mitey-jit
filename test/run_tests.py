import os
import subprocess
import sys

IGNORE_LIST = []

test_dir = os.path.dirname(os.path.realpath(__file__))

passed = 0
soft_passed = 0
failed = 0

for file in os.listdir("core"):
    if file.endswith(".wast"):
        without_wast = file[: -len(".wast")]
        if without_wast in IGNORE_LIST:
            continue

        input_path = os.path.join(test_dir, "core", file)
        output_path = os.path.join(
            test_dir, "core/wast-json", f"{without_wast}-wast.json"
        )
        if os.system(f"wast2json {input_path} -o {output_path}") != 0:
            print(f"Failed to convert {input_path} to wast-json")
            continue

        print(f"Testing {file}")
        result = subprocess.run(["./executor", output_path], capture_output=True)
        print(result.stdout.decode("utf-8"))
        print(result.stderr.decode("utf-8"), file=sys.stderr)

        try:
            p, sp, f = result.stdout.decode("utf-8").split("\n")[-4:-1]
            passed += int(p[len("Passes: ") :])
            soft_passed += int(sp[len("Soft passes: ") :])
            failed += int(f[len("Failures: ") :])
        except:
            print("failed to parse result of", file)
            failed += 1

print(f"Passed: {passed}, Soft passes: {soft_passed}, Failed: {failed}")
