import subprocess

def run_command(command):
    p = subprocess.Popen(command, shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    for line in iter(p.stdout.readline,''):
        print line.rstrip()
    return p.communicate()


while True:
    run_command('cabal run')
