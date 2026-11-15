# Pemilahan temuan tinjauan artikel DSRM (2026-11-15)

Sumber: tiga tinjauan independen atas artikel *Pembangunan Paket R cyclind …:
Penerapan DSRM* (tinjauan isi 32 temuan, literatur 20 temuan, analisis 22
temuan). Para peninjau adalah agen AI terpisah dari penulis artikel. Berkas
tinjauan berada di folder artikel (di luar repo ini).

Kategori:

- **(a)** masalah kode, uji, atau dokumentasi cyclind yang diperbaiki di cabang
  `fix-dsrm-review`;
- **(b)** masalah penulisan artikel saja, ditangani penulis artikel;
- **(c)** keputusan desain atau keterbatasan yang cukup dicatat di sini.

Ringkasnya: 9 temuan perlu perbaikan cyclind (a), 4 dicatat sebagai keputusan
atau keterbatasan (c), dan selebihnya masalah penulisan artikel (b).

## (a) Diperbaiki di cyclind

| Temuan | Masalah | Perbaikan |
|---|---|---|
| Analisis 1, isi 1 dan 8 | Klaim reproduksi CLI OECD Indonesia tertulis 0,9999 dan tanpa cakupan. Nilai sebenarnya 0,999833 (dihitung dari *fixture* `oecd-idn.rds`; CSV membulatkannya ke 1,000), hanya untuk agregasi tujuh siklus komponen terbitan OECD, dengan pilihan rumus dan amplitudo yang ditetapkan pada data yang sama. Metode lengkap dari empat seri asli: r = 0,880, 18 dari 24 titik balik (baris B1). | NEWS 0.8.0 dan 0.8.0.9000, `vignette("cyclind")` bagian 8, `?oecd_spec`, README. |
| Analisis 2, isi 11 | "*Leading* palsu ≤ 1,3 persen" adalah pangsa seri bukan-*leading* (bukan seri gangguan), hanya untuk `cli_spec()`, dan ambang disetel serta dinilai pada DGP yang sama. | `?cli_spec`, vinyet bagian 4 (galat baku Monte Carlo, catatan optimistis), README: angka `oecd_spec()` 2,5–7,5 persen dan bentuk percepatan sampai 2,1 persen. |
| Analisis 3 | R CMD check gagal di macOS sejak 0.7.0 (`test-cycle-form.R:193`, toleransi 1e-12 terhadap berkas emas Linux; selisih sekitar 1e-12 karena sistem HP ber-*condition number* sekitar 2e6). Laporan gerbang usang (0.6.0). | Toleransi 1e-8 dengan alasan numerik di uji; kelas tetap dibandingkan persis. Gerbang dijalankan ulang (`harness/gate-report.md`). |
| Analisis 8, isi 10 | `oecd_spec()` disebut "seperti sistem OECD" pada semua tahap; ambang kesesuaian siklus (40 persen hilang/ekstra, *concordance* 5 persen, puncak korelasi silang positif) adalah operasionalisasi cyclind dan lebih ketat daripada praktik OECD (CLI terbitan OECD Indonesia: rasio ekstra 0,69; dari empat seri asli hanya suku bunga yang lolos). | `?Classifier`, vinyet bagian 8. |
| Analisis 10, isi 7 | Uji BBQ melaporkan ambang (≥ 45), bukan hasil (46 dari 50). | Uji memakai ≥ 46 dengan komentar; README menyebut 46 dari 50 dan alasan selisihnya. |
| Analisis 16 | "Presisi mesin" untuk replikasi *filter*; ujinya memakai toleransi relatif 1e-8. | README. |
| Analisis 20 | STATE.md menulis PR #8 masih menunggu tinjauan. | STATE.md diperbarui. |
| Isi 12 | Versi 0.8.0 tidak ada di `main`: PR #12 (FR-015) digabung ke cabang PR #11 sesudah cabang itu masuk `main` (selang 28 detik). | FR-015 digabung ulang ke `main` di cabang ini. Tag rilis dan arsip ber-DOI diputuskan pemelihara. |
| Literatur M6 | cyclind tidak punya entri rujukan. | `inst/CITATION`. |

## (c) Keputusan desain atau keterbatasan yang dicatat

- **Analisis 7 (keterluasan).** Penyaring, penanggal titik balik, agregator,
  pengklasifikasi, pencilan, perpanjangan ujung seri, dan bentuk siklus
  adalah kelas strategi. Penyesuaian musiman, transformasi, amplitudo, dan
  penggeseran *lag* adalah pilihan di `CliSpec`, bukan strategi; menambah
  bentuk siklus (0.7.0) juga mengubah inti (`cycles.R`, `spec.R`,
  `classes-core.R`, `presentation.R`, `aggregate.R`). Dokumentasi paket
  tidak mengklaim semua tahap dapat diganti tanpa mengubah inti.
- **Analisis 11 (ancaman validitas).** Simulasi memakai satu DGP (siklus
  empat tahun, tiga puluh tahun data) untuk menyetel dan menilai ambang;
  ukuran uji *concordance* *fixed-b* 8,5–11,3 persen pada n = 120
  (`inst/calibration/test-size.csv`); ukuran uji Toda–Yamamoto pada siklus
  tersaring hanya didokumentasikan (01-requirements, di luar cakupan).
  Vinyet bagian 4 kini menyebut keterbatasan DGP.
- **Literatur M3 (BCDating).** README menyebut paket itu diarsipkan dari
  CRAN pada Mei 2026, sesuai temuan peninjau literatur. Dari sesi ini, CRAN
  tidak dapat dibuka langsung, dan cermin r-universe masih menampilkan
  0.9.8, sehingga status itu tidak dapat dikonfirmasi ulang. cyclind tidak
  bergantung pada BCDating; berkas emas membekukan hasil BCDating 0.9.8.
- **Analisis 10 (replikasi bulanan).** Bry–Boschan bulanan hanya
  dicocokkan dengan Coinprofile pada satu seri; perluasan ke banyak seri dan
  DGP dicatat sebagai pekerjaan berikutnya.

## (b) Ditangani di artikel

Analisis 4–6, 9, 11 (penulisan), 12–15, 17–19, 21–22; isi 2–6, 9, 13–32;
literatur M1, M2, M4, M5, m1–m14. Contoh: regex status rumus di skrip
artikel, titik masuk DSRM, klaim kontribusi, keseimbangan aktivitas,
literatur tambahan, gaya sitasi, EYD.
