#!/usr/bin/env python3
"""Suite de tests différentiels : compilateur maison vs gcc.

Pour chaque fichier .c trouvé (récursivement) dans le dossier de tests :

  - fichiers hors d'un dossier "errors" : on compile avec gcc (référence) et avec
    notre compilateur, on exécute les deux binaires et on compare code de retour,
    stdout et stderr.
  - fichiers dans un dossier "errors" : notre compilateur doit ÉCHOUER.

Hypothèse : le compilateur lit le C sur stdin ou prend le fichier en argument,
et écrit de l'IR LLVM (texte) sur stdout. Il est ensuite compilé par clang.
Adapte compile_ours() si ton pipeline est différent.

Usage :
    ./run_tests.py                          # tout le dossier tests/
    ./run_tests.py tests/valid/add          # filtre par sous-chaîne de chemin
    ./run_tests.py -c _build/default/bin/main.exe -j 8 -v
"""

import argparse
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

GREEN, RED, YELLOW, RESET = "\033[32m", "\033[31m", "\033[33m", "\033[0m"


@dataclass
class Result:
    status: str  # PASS | FAIL | ERROR
    detail: str = ""


@dataclass
class Run:
    code: int
    stdout: str
    stderr: str


def run(cmd, timeout, stdin=None):
    """Lance une commande, ne lève jamais : un timeout devient code -999."""
    try:
        p = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout, stdin=stdin
        )
        return Run(p.returncode, p.stdout, p.stderr)
    except subprocess.TimeoutExpired:
        return Run(-999, "", f"TIMEOUT après {timeout}s")
    except OSError as e:
        return Run(-998, "", f"impossible de lancer {cmd[0]} : {e}")


def compile_reference(src: Path, out: Path, timeout):
    return run(["gcc", "-std=c11", "-w", "-o", str(out), str(src)], timeout)


def compile_ours(compiler, clang, src: Path, workdir: Path, timeout):
    """Notre compilateur : C -> IR (stdout) -> clang -> binaire."""
    ir_path = workdir / "ours.ll"
    exe_path = workdir / "ours"
    with open(src) as f:
        r = run(compiler, timeout, stdin=f)
    if r.code != 0:
        return Run(r.code, r.stdout, r.stderr), None
    ir_path.write_text(r.stdout)
    r2 = run([clang, "-w", str(ir_path), "-o", str(exe_path)], timeout)
    if r2.code != 0:
        return Run(r2.code, r2.stdout, "clang a rejeté l'IR :\n" + r2.stderr), None
    return r2, exe_path


def diff_field(name, ref, ours):
    if ref == ours:
        return None
    return f"{name} différent\n    gcc  : {ref!r}\n    nous : {ours!r}"


def test_valid(src: Path, compiler, clang, timeout) -> Result:
    with tempfile.TemporaryDirectory() as d:
        wd = Path(d)
        ref_exe = wd / "ref"

        rc = compile_reference(src, ref_exe, timeout)
        if rc.code != 0:
            return Result("ERROR", "gcc ne compile pas ce test :\n" + rc.stderr.strip())

        ours_compile, our_exe = compile_ours(compiler, clang, src, wd, timeout)
        if our_exe is None:
            return Result("FAIL", "vocc ne compile pas ce test :\n" + ours_compile.stderr.strip() + "\n")

        ref = run([str(ref_exe)], timeout)
        ours = run([str(our_exe)], timeout)

        problems = [
            p
            for p in (
                diff_field("code de retour", ref.code, ours.code),
                diff_field("stdout", ref.stdout, ours.stdout),
                diff_field("stderr", ref.stderr, ours.stderr),
            )
            if p
        ]
        if problems:
            return Result("FAIL", "\n  ".join(problems))
        return Result("PASS")


def test_error(src: Path, compiler, clang, timeout) -> Result:
    """Le programme est invalide : notre compilateur doit refuser."""
    with tempfile.TemporaryDirectory() as d:
        wd = Path(d)
        _, exe = compile_ours(compiler, clang, src, wd, timeout)
        if exe is None:
            return Result("PASS")
        return Result("FAIL", "la compilation a réussi alors qu'elle devait échouer")


def is_error_test(src: Path, root: Path) -> bool:
    return "errors" in src.relative_to(root).parts[:-1]


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("filter", nargs="?", default="", help="ne lancer que les tests dont le chemin contient cette chaîne")
    ap.add_argument("-d", "--tests-dir", default="tests", type=Path)
    ap.add_argument("-c", "--compiler", default="_build/install/default/bin/vocc",
                    help="exécutable du compilateur (défaut : %(default)s)")
    ap.add_argument("--clang", default="clang")
    ap.add_argument("-t", "--timeout", type=float, default=10.0)
    ap.add_argument("-j", "--jobs", type=int, default=4)
    ap.add_argument("-v", "--verbose", action="store_true", help="afficher aussi les PASS")
    ap.add_argument("-b", "--build", action="store_true",
                    help="lancer `dune build` avant les tests (abandon si le build échoue)")
    args = ap.parse_args()

    if args.build:
        print("dune build ...", flush=True)
        try:
            build = subprocess.run(["dune", "build"])
        except FileNotFoundError:
            sys.exit("dune introuvable dans le PATH (eval $(opam env) ?)")
        if build.returncode != 0:
            sys.exit(f"{RED}dune build a échoué, tests annulés{RESET}")


    if not args.tests_dir.is_dir():
        sys.exit(f"dossier introuvable : {args.tests_dir}")
    if not Path(args.compiler).exists():
        sys.exit(f"compilateur introuvable : {args.compiler} (lance `dune build` ou passe -c)")

    compiler = [args.compiler]
    sources = sorted(
        p for p in args.tests_dir.rglob("*.c") if args.filter in str(p)
    )
    if not sources:
        sys.exit("aucun test trouvé")

    def job(src):
        fn = test_error if is_error_test(src, args.tests_dir) else test_valid
        return src, fn(src, compiler, args.clang, args.timeout)

    counts = {"PASS": 0, "FAIL": 0, "ERROR": 0}
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        for src, res in pool.map(job, sources):
            counts[res.status] += 1
            color = {"PASS": GREEN, "FAIL": RED, "ERROR": YELLOW}[res.status]
            if res.status != "PASS" or args.verbose:
                print(f"{color}{res.status}{RESET} {src}")
                if res.detail:
                    print("  " + res.detail.replace("\n", "\n  "))

    total = len(sources)
    print(f"\n{total} tests : {GREEN}{counts['PASS']} PASS{RESET}, "
          f"{RED}{counts['FAIL']} FAIL{RESET}, {YELLOW}{counts['ERROR']} ERROR{RESET}")
    sys.exit(1 if counts["FAIL"] or counts["ERROR"] else 0)


if __name__ == "__main__":
    main()
