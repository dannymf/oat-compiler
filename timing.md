**Timing Analysis**

Processor: Apple M1 Pro
OS: macOS 14.4.1

- matmul.ll
  + baseline: 1.94s user 0.00s system 99% cpu 1.940 total
  + greedy: 1.31s user 0.00s system 99% cpu 1.317 total
  + better: 0.91s user 0.00s system 99% cpu 0.912 total
  + clang: 0.07s user 0.00s system 98% cpu 0.077 total

  + baseline 01: 1.97s user 0.00s system 99% cpu 1.978 total
  + greedy 01: 1.32s user 0.00s system 99% cpu 1.331 total
  + better 01: 0.53s user 0.00s system 99% cpu 0.531 total
  + clang 01: 0.07s user 0.00s system 99% cpu 0.078 total

- regalloctest.oat
  + baseline: 0.61s user 0.00s system 99% cpu 0.610 total
  + greedy: 0.18s user 0.00s system 99% cpu 0.189 total
  + better: 0.19s user 0.00s system 99% cpu 0.197 total
  + clang: 0.01s user 0.00s system 94% cpu 0.017 total

  + baseline 01: 0.61s user 0.00s system 99% cpu 0.615 total
  + greedy 01: 0.58s user 0.00s system 99% cpu 0.584 total
  + better 01: 0.20s user 0.00s system 98% cpu 0.207 total
  + clang 01: 0.01s user 0.00s system 94% cpu 0.010 total

- largetwosum.oat
  + baseline: 2.00s user 0.01s system 99% cpu 2.008 total
  + greedy: 0.87s user 0.00s system 99% cpu 0.879 total
  + better: 0.75s user 0.01s system 99% cpu 0.755 total
  + clang: 0.40s user 0.01s system 99% cpu 0.403 total

  + baseline 01: 2.00s user 0.00s system 99% cpu 2.004 total
  + greedy 01: 0.82s user 0.00s system 99% cpu 0.823 total
  + better 01: 0.76s user 0.00s system 99% cpu 0.766 total
  + clang 01: 0.39s user 0.01s system 99% cpu 0.403 total

