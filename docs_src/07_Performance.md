# Performance

## Speed

Printing the balance for **~9500 transfers** spread over **~10 files**
on a MacBook Pro (14-inch, 2024) executed with [Bun](https://bun.com):

```txt
Benchmark 1: bunx transity balance journals/*.yaml
  Time (mean ± σ):     335.4 ms ±   4.3 ms    [User: 611.6 ms, System: 66.2 ms]
  Range (min … max):   329.9 ms … 342.7 ms    10 runs
```


## Size

The size of the bundled version for the
[transity.ad-si.com](https://transity.ad-si.com) website is around **62 kB**.
