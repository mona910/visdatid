# visdatid

<!-- badges: start -->
<!-- badges: end -->

`visdatid` adalah package R untuk diagnosis awal kualitas data melalui
visualisasi missing value, distribusi numerik, korelasi, serta laporan
masalah dan rekomendasi prapemrosesan.

Package ini menyediakan empat fungsi utama:

- `vis_miss_adv()` untuk diagnosis dan visualisasi missing value;
- `vis_cor_adv()` untuk visualisasi korelasi beserta signifikansi;
- `vis_distribution()` untuk diagnosis distribusi variabel numerik;
- `quality_report()` untuk menghasilkan tabel masalah kualitas data.

## Attribution

`visdatid` merupakan karya modifikasi dan pengembangan yang mengadopsi
konsep visualisasi awal dari package
[`visdat`](https://github.com/ropensci/visdat).

Package asli `visdat` dikembangkan oleh Nicholas Tierney dan para
kontributornya. Pengembangan pada `visdatid` tidak dimaksudkan sebagai
pengganti package asli, tetapi sebagai perluasan untuk keperluan diagnosis
kualitas data, evaluasi distribusi, analisis korelasi, serta rekomendasi
prapemrosesan.

Pengembangan utama meliputi:

1. diagnosis missing value berdasarkan ambang dan kelompok;
2. sampling sistematis untuk visualisasi data berukuran besar;
3. diagnosis skewness, kurtosis, dan pencilan;
4. korelasi dengan adjusted p-value dan penanda hubungan kuat;
5. laporan kualitas data dengan tingkat keparahan dan rekomendasi.

## Installation

Package dapat dipasang dari GitHub dengan perintah:

```r
install.packages("remotes")
remotes::install_github("mona910/visdatid")
