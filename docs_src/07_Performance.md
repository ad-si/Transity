# Performance

## Speed

Printing the balance for **~9500 transfers** spread over **~10 files**
on a MacBook Pro (14-inch, 2024) executed with [Bun](https://bun.com):

```txt
Benchmark 1: transity balance journals/*.yaml
  Time (mean ± σ):      96.4 ms ±   1.3 ms    [User: 89.6 ms, System: 5.7 ms]
  Range (min … max):    94.4 ms …  99.9 ms    29 runs
```


## Size

The size of the gzipped bundled version for the
[transity.ad-si.com](https://transity.ad-si.com) website is around **150 kB**.
