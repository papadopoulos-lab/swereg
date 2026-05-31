set pagination off
set width 0
set confirm off
run
python
# After `run` returns: if the inferior still has threads it stopped at a signal
# (crash) rather than exiting -> dump the backtrace and exit non-zero (134) so
# processx/.check_worker_error reports it. Clean exit -> exit 0.
inf = gdb.inferiors()[0]
if inf.threads():
    print("\n===== SWEREG-GDB: inferior STOPPED at signal — backtrace =====")
    gdb.execute("bt full")
    print("\n----- all threads -----")
    gdb.execute("thread apply all bt")
    print("\n----- registers -----")
    gdb.execute("info registers rip rsp rbp rax rbx rdi rsi")
    print("\n----- disasm @pc -----")
    gdb.execute("x/16i $pc")
    gdb.execute("quit 134")
else:
    gdb.execute("quit 0")
end
