// Numerics for the Gaussian-copula post. Plain ES module so it runs in both
// Observable (via the OJS cells in index.qmd) and Node (`node stats.js` runs
// the self-check at the bottom).

// ---------------------------------------------------------------- normal law

// Acklam's inverse normal CDF. Relative error < 1.15e-9 over the whole range.
const A = [-3.969683028665376e1, 2.209460984245205e2, -2.759285104469687e2,
           1.38357751867269e2, -3.066479806614716e1, 2.506628277459239];
const B = [-5.447609879822406e1, 1.615858368580409e2, -1.556989798598866e2,
           6.680131188771972e1, -1.328068155288572e1];
const C = [-7.784894002430293e-3, -3.223964580411365e-1, -2.400758277161838,
           -2.549732539343734, 4.374664141464968, 2.938163982698783];
const D = [7.784695709041462e-3, 3.224671290700398e-1, 2.445134137142996,
           3.754408661907416];

export function qnorm(p) {
  if (p <= 0) return -Infinity;
  if (p >= 1) return Infinity;
  const pLow = 0.02425;
  let q, r;
  if (p < pLow) {
    q = Math.sqrt(-2 * Math.log(p));
    return (((((C[0] * q + C[1]) * q + C[2]) * q + C[3]) * q + C[4]) * q + C[5]) /
           ((((D[0] * q + D[1]) * q + D[2]) * q + D[3]) * q + 1);
  }
  if (p > 1 - pLow) {
    q = Math.sqrt(-2 * Math.log(1 - p));
    return -(((((C[0] * q + C[1]) * q + C[2]) * q + C[3]) * q + C[4]) * q + C[5]) /
            ((((D[0] * q + D[1]) * q + D[2]) * q + D[3]) * q + 1);
  }
  q = p - 0.5;
  r = q * q;
  return (((((A[0] * r + A[1]) * r + A[2]) * r + A[3]) * r + A[4]) * r + A[5]) * q /
         (((((B[0] * r + B[1]) * r + B[2]) * r + B[3]) * r + B[4]) * r + 1);
}

// Normal CDF via Numerical Recipes' erfcc. Max absolute error ~1.5e-8, which
// is well past what any of these plots can show.
export function pnorm(x) {
  const z = Math.abs(x) / Math.SQRT2;
  const t = 1 / (1 + 0.5 * z);
  const ans = t * Math.exp(-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 +
    t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 +
    t * (1.48851587 + t * (-0.82215223 + t * 0.17087277)))))))));
  const erfc = x >= 0 ? ans : 2 - ans;
  return 1 - 0.5 * erfc;
}

// ------------------------------------------------------------------- sampling

// mulberry32: tiny seeded PRNG. Seeded so a slider drag redraws the *same*
// sample and the reader sees the estimator move, not the noise.
export function makeRng(seed) {
  let a = seed >>> 0;
  return function () {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// Standard normal via inverse transform (Box-Muller would need pair caching and
// this is already exact enough).
export function rnorm(rng) {
  return qnorm(rng() * 0.999998 + 1e-6);
}

// n draws from a bivariate standard normal with correlation rho.
export function rbinorm(n, rho, rng) {
  const out = new Array(n);
  const s = Math.sqrt(1 - rho * rho);
  for (let i = 0; i < n; i++) {
    const z1 = rnorm(rng);
    const z2 = rho * z1 + s * rnorm(rng);
    out[i] = [z1, z2];
  }
  return out;
}

// ------------------------------------------------------- copula transform

// Ranks, 1-based, ties averaged.
export function ranks(x) {
  const idx = x.map((v, i) => i).sort((a, b) => x[a] - x[b]);
  const r = new Array(x.length);
  let i = 0;
  while (i < idx.length) {
    let j = i;
    while (j + 1 < idx.length && x[idx[j + 1]] === x[idx[i]]) j++;
    const avg = (i + j) / 2 + 1;
    for (let k = i; k <= j; k++) r[idx[k]] = avg;
    i = j + 1;
  }
  return r;
}

// The Park & Gupta copula term: P* = Phi^-1(H(P)), with H the rescaled
// empirical CDF rank/(n+1). The R side uses a kernel-smoothed CDF by default;
// rank/(n+1) is the "rank.n1" option and keeps this readable.
export function normalScores(x) {
  const n = x.length;
  return ranks(x).map((r) => qnorm(r / (n + 1)));
}

// ------------------------------------------------------------------- linear

// Solve (X'X) b = X'y and return coefficients, standard errors and R^2.
// X is an array of rows, each row already including the intercept column.
export function ols(X, y) {
  const n = X.length, k = X[0].length;
  const xtx = Array.from({ length: k }, () => new Float64Array(k));
  const xty = new Float64Array(k);
  for (let i = 0; i < n; i++) {
    const row = X[i];
    for (let a = 0; a < k; a++) {
      xty[a] += row[a] * y[i];
      for (let b = a; b < k; b++) xtx[a][b] += row[a] * row[b];
    }
  }
  for (let a = 0; a < k; a++) for (let b = 0; b < a; b++) xtx[a][b] = xtx[b][a];

  const inv = invert(xtx, k);
  if (!inv) return null;

  const beta = new Float64Array(k);
  for (let a = 0; a < k; a++) {
    let s = 0;
    for (let b = 0; b < k; b++) s += inv[a][b] * xty[b];
    beta[a] = s;
  }

  let rss = 0, tss = 0;
  const ybar = mean(y);
  const resid = new Float64Array(n);
  for (let i = 0; i < n; i++) {
    let fit = 0;
    for (let a = 0; a < k; a++) fit += X[i][a] * beta[a];
    resid[i] = y[i] - fit;
    rss += resid[i] * resid[i];
    tss += (y[i] - ybar) * (y[i] - ybar);
  }
  const df = n - k;
  const sigma2 = rss / df;
  const se = Array.from({ length: k }, (_, a) => Math.sqrt(sigma2 * inv[a][a]));

  return { beta: Array.from(beta), se, sigma: Math.sqrt(sigma2),
           r2: 1 - rss / tss, resid: Array.from(resid), df };
}

// Gauss-Jordan with partial pivoting. Returns null on a singular matrix, which
// is exactly what a normally distributed P produces -- see the post.
function invert(m, k) {
  const a = Array.from({ length: k }, (_, i) =>
    Float64Array.from([...m[i], ...Array.from({ length: k }, (_, j) => (i === j ? 1 : 0))]));
  for (let col = 0; col < k; col++) {
    let piv = col;
    for (let r = col + 1; r < k; r++) if (Math.abs(a[r][col]) > Math.abs(a[piv][col])) piv = r;
    if (Math.abs(a[piv][col]) < 1e-12) return null;
    [a[col], a[piv]] = [a[piv], a[col]];
    const d = a[col][col];
    for (let c = 0; c < 2 * k; c++) a[col][c] /= d;
    for (let r = 0; r < k; r++) {
      if (r === col) continue;
      const f = a[r][col];
      if (f === 0) continue;
      for (let c = 0; c < 2 * k; c++) a[r][c] -= f * a[col][c];
    }
  }
  return a.map((row) => Array.from(row.slice(k)));
}

// ------------------------------------------------------------- descriptives

export const mean = (x) => x.reduce((s, v) => s + v, 0) / x.length;

export function sd(x) {
  const m = mean(x);
  return Math.sqrt(x.reduce((s, v) => s + (v - m) * (v - m), 0) / (x.length - 1));
}

export function skewness(x) {
  const m = mean(x), n = x.length;
  let m2 = 0, m3 = 0;
  for (const v of x) { const d = v - m; m2 += d * d; m3 += d * d * d; }
  m2 /= n; m3 /= n;
  return m3 / Math.pow(m2, 1.5);
}

export function exKurtosis(x) {
  const m = mean(x), n = x.length;
  let m2 = 0, m4 = 0;
  for (const v of x) { const d = v - m; m2 += d * d; m4 += d * d * d * d; }
  m2 /= n; m4 /= n;
  return m4 / (m2 * m2) - 3;
}

export function corr(x, y) {
  const mx = mean(x), my = mean(y);
  let sxy = 0, sxx = 0, syy = 0;
  for (let i = 0; i < x.length; i++) {
    const dx = x[i] - mx, dy = y[i] - my;
    sxy += dx * dy; sxx += dx * dx; syy += dy * dy;
  }
  return sxy / Math.sqrt(sxx * syy);
}

export const standardize = (x) => { const m = mean(x), s = sd(x); return x.map((v) => (v - m) / s); };

// -------------------------------------------------------------- marginals

// Every marginal is a strictly increasing transform of a standard normal draw.
// That is the whole point of widget 1: a monotone transform cannot change the
// copula, so the middle panel is frozen while the left panel changes shape.
export const MARGINALS = {
  normal:      { label: "Normal",          g: (z) => z },
  lognormal:   { label: "Lognormal",       g: (z) => Math.exp(z) },
  exponential: { label: "Exponential",     g: (z) => -Math.log(1 - pnorm(z) * 0.999999) },
  uniform:     { label: "Uniform",         g: (z) => pnorm(z) },
  bimodal:     { label: "Bimodal",         g: (z) => z + 1.6 * Math.tanh(4 * z) },
  heavytail:   { label: "Heavy-tailed",    g: (z) => Math.sinh(1.2 * z) }
};

// ------------------------------------------------- the two estimators

// Simulate y = b0 + b1*P + b2*W + xi, where P is endogenous through a Gaussian
// copula with correlation rho, and P's marginal is lognormal with log-sd sigma.
// sigma -> 0 makes P normal, which is where identification dies.
export function simulate({ n, rho, sigma, seed, b0 = 1, b1 = 2, b2 = 0.5, sdXi = 1 }) {
  const rng = makeRng(seed);
  const zz = rbinorm(n, rho, rng);
  const P = [], W = [], xi = [];
  for (let i = 0; i < n; i++) {
    P.push(Math.exp(sigma * zz[i][0]));
    xi.push(sdXi * zz[i][1]);
    W.push(rnorm(rng));
  }
  const Ps = standardize(P);
  const y = Ps.map((p, i) => b0 + b1 * p + b2 * W[i] + xi[i]);
  return { P: Ps, W, xi, y, truth: { b0, b1, b2 } };
}

// Naive OLS and the Park & Gupta augmented regression on the same sample.
export function fitBoth({ P, W, y }) {
  const naive = ols(P.map((p, i) => [1, p, W[i]]), y);
  const cop = normalScores(P);
  const pg = ols(P.map((p, i) => [1, p, W[i], cop[i]]), y);
  return { naive, pg, cop };
}

// omega = 1 - R^2 of the copula term on the other regressors. The package
// reports this; near zero means the copula term carries no independent
// variation and nothing is identified.
export function omega(P, W, cop) {
  const fit = ols(P.map((p, i) => [1, p, W[i]]), cop);
  return fit ? 1 - fit.r2 : 0;
}

// ------------------------------------------------------------- self-check

function demo() {
  const ok = (name, cond) => {
    if (!cond) throw new Error("FAIL: " + name);
    console.log("  ok  " + name);
  };
  const close = (a, b, tol) => Math.abs(a - b) < tol;

  console.log("normal law");
  ok("qnorm(0.975)", close(qnorm(0.975), 1.959964, 1e-5));
  ok("qnorm(0.025)", close(qnorm(0.025), -1.959964, 1e-5));
  ok("qnorm(0.5)", close(qnorm(0.5), 0, 1e-12));
  ok("qnorm(0.99)", close(qnorm(0.99), 2.326348, 1e-5));
  ok("qnorm(0.001)", close(qnorm(0.001), -3.090232, 1e-4));
  ok("pnorm(1.96)", close(pnorm(1.96), 0.9750021, 1e-6));
  ok("pnorm(0)", close(pnorm(0), 0.5, 1e-7));
  ok("pnorm(1)", close(pnorm(1), 0.8413447461, 1e-7));
  ok("pnorm(-3)", close(pnorm(-3), 0.001349898, 1e-7));
  for (const u of [0.01, 0.2, 0.5, 0.8, 0.99]) {
    ok(`round trip u=${u}`, close(pnorm(qnorm(u)), u, 1e-6));
  }

  console.log("ols");
  // Noiseless data: coefficients must come back exactly.
  const Xe = [[1, 1, 2], [1, 2, 1], [1, 3, 5], [1, 4, 3], [1, 7, 2], [1, 9, 8]];
  const ye = Xe.map((r) => 3 + 2 * r[1] - 1.5 * r[2]);
  const fe = ols(Xe, ye);
  ok("intercept", close(fe.beta[0], 3, 1e-8));
  ok("slope 1", close(fe.beta[1], 2, 1e-8));
  ok("slope 2", close(fe.beta[2], -1.5, 1e-8));
  ok("r2 == 1", close(fe.r2, 1, 1e-9));
  // Simple regression SE against the textbook formula.
  const x = [1, 2, 3, 4, 5, 6, 7, 8];
  const yv = [2.1, 3.9, 6.2, 7.8, 10.1, 12.2, 13.8, 16.1];
  const f2 = ols(x.map((v) => [1, v]), yv);
  const mx = mean(x);
  const sxx = x.reduce((s, v) => s + (v - mx) * (v - mx), 0);
  ok("slope se", close(f2.se[1], f2.sigma / Math.sqrt(sxx), 1e-9));
  ok("singular matrix returns null", ols([[1, 1], [1, 1], [1, 1]], [1, 2, 3]) === null);

  console.log("copula transform");
  const raw = [5, 1, 3, 2, 4];
  ok("ranks", JSON.stringify(ranks(raw)) === JSON.stringify([5, 1, 3, 2, 4]));
  ok("ties averaged", JSON.stringify(ranks([1, 1, 3])) === JSON.stringify([1.5, 1.5, 3]));
  // Rank-based, so any increasing transform leaves the copula term untouched.
  const ns1 = normalScores([1, 2, 3, 4, 5]);
  const ns2 = normalScores([1, 2, 3, 4, 5].map((v) => Math.exp(3 * v)));
  ok("monotone invariance", ns1.every((v, i) => close(v, ns2[i], 1e-12)));
  ok("symmetric about 0", close(mean(ns1), 0, 1e-9));

  console.log("descriptives");
  ok("lognormal is right-skewed", skewness(Array.from({ length: 4000 },
      (_, i) => Math.exp(qnorm((i + 0.5) / 4000)))) > 3);
  ok("normal has ~0 skew", Math.abs(skewness(Array.from({ length: 4000 },
      (_, i) => qnorm((i + 0.5) / 4000)))) < 0.05);
  ok("corr with self", close(corr([1, 2, 3, 4], [1, 2, 3, 4]), 1, 1e-12));
  ok("corr sign flips", close(corr([1, 2, 3, 4], [4, 3, 2, 1]), -1, 1e-12));

  console.log("the actual method");
  // Strongly skewed P, strong endogeneity: PG should beat naive OLS badly.
  const d = simulate({ n: 4000, rho: 0.6, sigma: 1.0, seed: 7 });
  const { naive, pg } = fitBoth(d);
  const errNaive = Math.abs(naive.beta[1] - 2);
  const errPg = Math.abs(pg.beta[1] - 2);
  console.log(`      naive b1 = ${naive.beta[1].toFixed(4)}  (err ${errNaive.toFixed(4)})`);
  console.log(`      PG    b1 = ${pg.beta[1].toFixed(4)}  (err ${errPg.toFixed(4)})`);
  ok("naive OLS is biased upward", naive.beta[1] > 2.15);
  ok("PG lands near the truth", errPg < 0.1);
  ok("PG beats naive by a wide margin", errPg < errNaive / 3);
  ok("copula coefficient is positive when rho > 0", pg.beta[3] > 0);

  // gamma = rho * sigma_xi. Structural sigma is ~1 here by construction.
  console.log(`      gamma = ${pg.beta[3].toFixed(4)}, rho*sigma_xi = 0.6`);
  ok("gamma recovers rho*sigma_xi", close(pg.beta[3], 0.6, 0.15));

  console.log("the identification cliff");
  // As P approaches normal, the copula term collapses onto P itself.
  const skewed = simulate({ n: 2000, rho: 0.6, sigma: 1.0, seed: 11 });
  const nearNormal = simulate({ n: 2000, rho: 0.6, sigma: 0.02, seed: 11 });
  const oSkew = omega(skewed.P, skewed.W, fitBoth(skewed).cop);
  const oNorm = omega(nearNormal.P, nearNormal.W, fitBoth(nearNormal).cop);
  console.log(`      omega: skewed = ${oSkew.toFixed(4)}, near-normal = ${oNorm.toFixed(6)}`);
  ok("skewed P is identified", oSkew > 0.05);
  ok("near-normal P is not", oNorm < 0.005);
  ok("omega collapses", oNorm < oSkew / 20);

  const seSkew = fitBoth(skewed).pg.se[1];
  const seNorm = fitBoth(nearNormal).pg.se[1];
  console.log(`      se(b1): skewed = ${seSkew.toFixed(4)}, near-normal = ${seNorm.toFixed(4)}`);
  ok("standard error explodes as P goes normal", seNorm > 8 * seSkew);

  console.log("marginals");
  for (const [key, m] of Object.entries(MARGINALS)) {
    const zs = [-2, -1, -0.5, 0, 0.5, 1, 2];
    const gs = zs.map(m.g);
    ok(`${key} is strictly increasing`, gs.every((v, i) => i === 0 || v > gs[i - 1]));
    ok(`${key} is finite`, gs.every(Number.isFinite));
  }
  // Monotone transforms leave the copula term identical across marginals.
  const rngA = makeRng(3);
  const base = rbinorm(300, 0.5, rngA).map((p) => p[0]);
  const refScores = normalScores(base.map(MARGINALS.normal.g));
  for (const [key, m] of Object.entries(MARGINALS)) {
    const s = normalScores(base.map(m.g));
    ok(`${key} shares the copula`, s.every((v, i) => close(v, refScores[i], 1e-12)));
  }

  console.log("\nall checks passed");
}

if (typeof process !== "undefined" && process.argv?.[1]?.endsWith("stats.js")) demo();
