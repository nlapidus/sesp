N <- 1e3
v <- c(rmultinom(1, N, runif(4)))

.tp <- v[1] / N
.tn <- v[2] / N
.fp <- v[3] / N
.fn <- v[4] / N

.pos <- .tp + .fp
.neg <- .tn + .fn
.freq <- .tp + .fn

.se <- .tp / .freq
.sp <- .tn / (1 - .freq)
.ppv <- .tp / .pos
.npv <- .tn / .neg


all(
  se(sp = .sp, ppv = .ppv, npv = .npv) == .se,
  se(sp = .sp, ppv = .ppv, freq = .freq) == .se,
  se(sp = .sp, npv = .npv, freq = .freq) == .se,
  se(ppv = .ppv, npv = .npv, freq = .freq) == .se,

  sp(se = .se, ppv = .ppv, npv = .npv) == .sp,
  sp(se = .se, ppv = .ppv, freq = .freq) == .sp,
  sp(se = .se, npv = .npv, freq = .freq) == .sp,
  sp(ppv = .ppv, npv = .npv, freq = .freq) == .sp,

  freq(se = .se, sp = .sp, ppv = .ppv) == .freq,
  freq(se = .se, sp = .sp, npv = .npv) == .freq,
  freq(se = .se, ppv = .ppv, npv = .npv) == .freq,
  freq(sp = .sp, ppv = .ppv, npv = .npv) == .freq,

  ppv(se = .se, sp = .sp, freq = .freq) == .ppv,
  ppv(se = .se, sp = .sp, npv = .npv) == .ppv,
  ppv(se = .se, freq = .freq, npv = .npv) == .ppv,
  ppv(sp = .sp, freq = .freq, npv = .npv) == .ppv,

  npv(se = .se, sp = .sp, freq = .freq) == .npv,
  npv(se = .se, sp = .sp, ppv = .ppv) == .npv,
  npv(se = .se, freq = .freq, ppv = .ppv) == .npv,
  npv(sp = .sp, freq = .freq, ppv = .ppv) == .npv
)
