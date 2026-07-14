# visdatid

<!-- badges: start -->
<!-- badges: end -->

`visdatid` adalah package R untuk diagnosis awal kualitas data melalui
visualisasi *missing value*, distribusi numerik, korelasi, serta laporan
masalah dan rekomendasi prapemrosesan.

Package ini menyediakan empat fungsi utama:

- `vis_miss_adv()` untuk diagnosis dan visualisasi *missing value*;
- `vis_cor_adv()` untuk visualisasi korelasi beserta signifikansinya;
- `vis_distribution()` untuk diagnosis distribusi variabel numerik;
- `quality_report()` untuk menghasilkan tabel masalah kualitas data.

## Attribution

`visdatid` merupakan karya modifikasi dan pengembangan yang mengadopsi
konsep visualisasi awal dari package
[`visdat`](https://github.com/ropensci/visdat).

Package asli `visdat` dikembangkan oleh Nicholas Tierney dan para
kontributornya. Pengembangan `visdatid` tidak dimaksudkan untuk menggantikan
package asli, tetapi untuk memperluas fungsi diagnosis kualitas data,
evaluasi distribusi, dan analisis korelasi.

Pengembangan utama meliputi:

1. diagnosis *missing value* berdasarkan ambang dan kelompok;
2. sampling sistematis untuk visualisasi data berukuran besar;
3. diagnosis skewness, bentuk distribusi, dan pencilan;
4. korelasi dengan *adjusted p-value* dan penanda korelasi kuat;
5. laporan kualitas data dengan tingkat keparahan dan rekomendasi.

## Installation

Package dapat dipasang langsung dari GitHub dengan perintah:

```r
install.packages("remotes")
remotes::install_github("mona910/visdatid")
```

## Contoh penggunaan

```r
library(visdatid)

quality_report(airquality)
vis_distribution(mtcars, variable = "hp")
```

Contoh analisis korelasi:

```r
vis_cor_adv(
  mtcars,
  method = "auto",
  adjust_method = "BH"
)
```

Contoh visualisasi *missing value*:

```r
vis_miss_adv(
  airquality,
  threshold = 10
)
```

## Hasil pemeriksaan package

Package telah diperiksa menggunakan `devtools::check()` dengan hasil:

```text
0 errors
0 warnings
1 note
```

Satu note yang muncul adalah:

```text
checking for future file timestamps ...
unable to verify current time
```

Note tersebut berkaitan dengan ketidakmampuan lingkungan sistem memverifikasi
waktu saat ini dan tidak berkaitan dengan fungsi atau source code package.

Pengujian otomatis menghasilkan:

```text
FAIL 0
WARN 0
SKIP 0
PASS 219
```

Output lengkap pemeriksaan tersedia pada
[`check_result.txt`](check_result.txt).

## Batas penggunaan

`visdatid` digunakan untuk diagnosis dan rekomendasi awal. Package ini tidak
melakukan imputasi, transformasi, penghapusan pencilan, atau pembersihan data
secara otomatis.
