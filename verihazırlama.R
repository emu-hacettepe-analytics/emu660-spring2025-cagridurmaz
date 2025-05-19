# ================================================================
# VERI HAZIRLAMA BETİĞİ (veri_hazirla.R)
# ================================================================
# Amaç: Ham CSV dosyalarını okumak, ID'leri standartlaştırmak,
#       sahte ID'ler oluşturmak, test kullanıcılarını silmek
#       ve sonuçları RDS olarak kaydetmek.
# ================================================================

# Başlangıç zamanı
start_time <- Sys.time()

# --- Gerekli Kütüphaneler ---
cat("Gerekli kütüphaneler yükleniyor...\n")
# install.packages(c("readr", "dplyr", "purrr", "tibble")) # Eğer eksikse ilk seferde çalıştırın
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(tibble)
})

# --- 1. Ayarlar ---
cat("\n--- 1. Ayarlar Yapılıyor ---\n")
dosya_yolu <- "~/Documents/GitHub/emu660-spring2025-cagridurmaz/myemu-april-2025-datatables/"
ogrenci_sutunlari <- c("user_id", "userid", "studentid", "student_id", "username", "ogrenci_no",
                       "studentno","student", "stdid")
standard_id_sutun_adi <- "standardize_number"
sahte_id_sutun_adi <- "fake_student_id"
sahte_id_baslangic <- 90000001

# --- 2. CSV Dosyalarını Listeleme ve Okuma ---
cat("\n--- 2. CSV Dosyaları Okunuyor ---\n")
dosyalar <- list.files(dosya_yolu, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
veri_listesi <- list()
if (length(dosyalar) == 0) {
  warning("Belirtilen klasörde ('", dosya_yolu, "') CSV dosyası bulunamadı.")
} else {
  cat("Bulunan CSV dosyaları okunuyor...\n")
  veri_listesi <- map(setNames(dosyalar, tolower(gsub("\\.csv$", "", basename(dosyalar), ignore.case = TRUE))),
                      ~tryCatch(read_csv(.x, col_types = cols(.default = "c"), show_col_types = FALSE, name_repair = "minimal"),
                                error = function(e) { cat("  ! HATA:", names(.x), "okunurken:", conditionMessage(e), "\n"); NULL }))
  veri_listesi <- compact(veri_listesi) # NULL olanları (okuma hatası verenleri) çıkar
  cat(length(veri_listesi), "adet CSV dosyası başarıyla okundu.\n")
}

# --- 3. Sadece Yeniden Adlandırma Yapan Fonksiyon ---
rename_student_id_column <- function(df, hedef_sutun_adi){
  # ... (Fonksiyon içeriği öncekiyle aynı, değişiklik yok) ...
  if (!is.data.frame(df)) return(df)
  mevcut_isimler_kucuk <- tolower(names(df))
  orijinal_isimler <- names(df)
  bulunan_isim_kucuk <- intersect(mevcut_isimler_kucuk, tolower(ogrenci_sutunlari))[1]
  if (!is.na(bulunan_isim_kucuk)) {
    orijinal_bulunan_isim <- orijinal_isimler[which(mevcut_isimler_kucuk == bulunan_isim_kucuk)[1]]
    if (hedef_sutun_adi %in% names(df) && orijinal_bulunan_isim != hedef_sutun_adi) { return(df) }
    if (orijinal_bulunan_isim == hedef_sutun_adi) return(df)
    df <- df %>% rename(!!hedef_sutun_adi := !!sym(orijinal_bulunan_isim))
  }
  return(df)
}

# --- 4. Orijinal ID Sütununu Standart İsimle Oluştur ---
veri_listesi_duzeltilmis <- list()
if (length(veri_listesi) > 0) {
  cat("\n--- 4. Öğrenci ID Sütunu Standartlaştırılıyor ---\n", sep="")
  veri_listesi_duzeltilmis <- map(veri_listesi, ~rename_student_id_column(.x, standard_id_sutun_adi))
  cat("Standartlaştırma tamamlandı.\n")
} else {
  cat("\n--- 4. Standartlaştırma Adımı Atlandı (Okunacak Veri Yok) ---\n")
}

# --- 5. Tüm Benzersiz Orijinal ID'leri Topla ---
benzersiz_orijinal_idler <- character(0)
if (length(veri_listesi_duzeltilmis) > 0) {
  cat("\n--- 5. Benzersiz Orijinal Öğrenci ID'leri Toplanıyor ---\n")
  safe_extract <- possibly(~ if(is.data.frame(.x) && standard_id_sutun_adi %in% names(.x)) unique(na.omit(.x[[standard_id_sutun_adi]])) else character(0), otherwise = character(0))
  benzersiz_orijinal_idler <- unique(unlist(map(veri_listesi_duzeltilmis, safe_extract)))
  cat(length(benzersiz_orijinal_idler), "adet benzersiz orijinal ID bulundu.\n")
} else {
  cat("\n--- 5. ID Toplama Adımı Atlandı ---\n")
}

# --- 6. Eşleştirme Tablosu Oluştur (Orijinal ID -> Sahte ID) ---
id_map <- NULL
if (length(benzersiz_orijinal_idler) > 0) {
  cat("\n--- 6. Sahte ID Eşleştirmesi Oluşturuluyor ---\n")
  sahte_idler <- as.character(seq(from = sahte_id_baslangic, length.out = length(benzersiz_orijinal_idler)))
  id_map <- setNames(sahte_idler, benzersiz_orijinal_idler)
  cat("Eşleştirme tamamlandı (", length(id_map), " adet ID eşleştirildi).\n", sep="")
} else {
  cat("\n--- 6. Eşleştirme Adımı Atlandı (Benzersiz Orijinal ID Bulunamadı) ---\n")
}

# --- 7. Sahte ID Sütununu Ekleme Fonksiyonu ---
add_fake_id_column <- function(df, mapping_vector, original_col, new_col){
  # ... (Fonksiyon içeriği öncekiyle aynı, değişiklik yok) ...
  if (!is.data.frame(df)) return(df)
  if (original_col %in% names(df) && !is.null(mapping_vector) && length(mapping_vector) > 0) {
    if (new_col %in% names(df)) return(df)
    df <- df %>% mutate(!!new_col := mapping_vector[ .data[[original_col]] ])
  }
  return(df)
}

# --- 8. Sahte ID Sütununu Tüm Veri Setlerine Ekle ---
veri_listesi_final <- list()
if (length(veri_listesi_duzeltilmis) > 0 && !is.null(id_map)) {
  cat("\n--- 8. Sahte Öğrenci ID Sütunu Ekleniyor ---\n", sep="")
  veri_listesi_final <- map(veri_listesi_duzeltilmis, ~add_fake_id_column(.x, id_map, standard_id_sutun_adi, sahte_id_sutun_adi))
  cat("Sahte ID ekleme işlemi tamamlandı.\n")
} else {
  cat("\n--- 8. Sahte ID Ekleme Adımı Atlandı ---\n")
  veri_listesi_final <- veri_listesi_duzeltilmis # Önceki adımı kullan
}

# --- 9. Test Kullanıcılarını Silme ---
cat("\n--- 9. Test Kullanıcılarının Kayıtları Siliniyor ---\n")
kaynak_liste_adi <- "veri_listesi_final"
yeni_liste_adi <- "veri_listesi_test_silinmis"
id_sutunu_adi_silme <- standard_id_sutun_adi
users_tablo_adi <- "users"

# Bu adımın çalışması için kaynak listenin var olması GEREKİR.
if (!exists(kaynak_liste_adi) || !is.list(get(kaynak_liste_adi))) {
  stop("HATA: Kaynak liste ('", kaynak_liste_adi, "') bulunamadı. Betik durduruldu.")
}
kaynak_liste <- get(kaynak_liste_adi)
veri_listesi_test_silinmis <- kaynak_liste # Başlangıçta kopyala

# users tablosu veya ID sütunu yoksa uyarı verip devam et (liste zaten kopyalandı)
if (!(users_tablo_adi %in% names(kaynak_liste))) {
  warning("UYARI: '", users_tablo_adi, "' tablosu bulunamadı, test kullanıcısı silme işlemi atlanıyor.")
} else {
  users_df_silme <- kaynak_liste[[users_tablo_adi]]
  if (!is.data.frame(users_df_silme)) {
    warning("UYARI: '", users_tablo_adi, "' bir veri çerçevesi değil, test kullanıcısı silme işlemi atlanıyor.")
  } else if (!(id_sutunu_adi_silme %in% names(users_df_silme))) {
    warning("UYARI: '", id_sutunu_adi_silme, "' sütunu '", users_tablo_adi, "' tablosunda yok, test kullanıcısı silme işlemi atlanıyor.")
  } else {
    # Silme işlemi burada yapılır
    cat("ID'si 'test' ile başlayan kullanıcılar tespit ediliyor ('", users_tablo_adi, "' tablosundan)...\n", sep="")
    silinecek_test_idler <- users_df_silme %>%
      filter(!is.na(.data[[id_sutunu_adi_silme]]) & startsWith(as.character(.data[[id_sutunu_adi_silme]]), "test")) %>%
      pull(.data[[id_sutunu_adi_silme]]) %>%
      unique()
    
    if (length(silinecek_test_idler) == 0) {
      cat("ID'si 'test' ile başlayan ve silinecek kayıt bulunamadı.\n")
      # veri_listesi_test_silinmis zaten kaynak listeye eşit, bir şey yapmaya gerek yok.
    } else {
      cat(length(silinecek_test_idler), " adet benzersiz 'test' ID'sine sahip kullanıcının tüm tablolardaki kayıtları silinecek.\n")
      veri_listesi_test_silinmis <- map(kaynak_liste, function(df) {
        if (is.data.frame(df) && (id_sutunu_adi_silme %in% names(df))) {
          df_filtrelenmis <- df %>% filter(!(.data[[id_sutunu_adi_silme]] %in% silinecek_test_idler))
          return(df_filtrelenmis)
        } else { return(df) }
      })
      cat("\nSilme işlemi tamamlandı. Temizlenmiş veriler '", yeni_liste_adi, "' listesinde.\n", sep="")
    }
  }
}

# --- 10. İşlenmiş Veriyi Kaydetme (RDS olarak) ---
# Not: Önceki numaralandırmada 14. adımdı.
cat("\n--- 10. İşlenmiş Veri Kaydediliyor ---\n")

# Kaydedilecek nihai listenin adı
nihai_kaydedilecek_liste_adi <- "veri_listesi_test_silinmis"

# Bu listenin var olduğundan emin olalım
if (!exists(nihai_kaydedilecek_liste_adi)) {
  stop("HATA: Kaydedilecek '", nihai_kaydedilecek_liste_adi, "' listesi oluşturulamadı!")
}
nihai_kaydedilecek_liste <- get(nihai_kaydedilecek_liste_adi)

# *** BURASI ÇOK ÖNEMLİ: Değişkenleri burada tanımlıyoruz ***
kayit_klasoru <- "islenmis_veri"
rds_dosya_adi <- file.path(kayit_klasoru, "temiz_veri_listesi.rds")
# *** ---------------------------------------------------- ***
cat("Kaydedilecek dosya yolu:", rds_dosya_adi, "\n")

# Klasör yoksa oluştur
if (!dir.exists(kayit_klasoru)) {
  cat("Klasör mevcut değil, oluşturuluyor:", kayit_klasoru, "\n")
  dir.create(kayit_klasoru, recursive = TRUE)
  cat("Klasör oluşturuldu.\n")
} else {
  cat("Klasör zaten mevcut:", kayit_klasoru, "\n")
}

# Listeyi RDS dosyası olarak kaydet
cat("Kaydediliyor...\n")
saveRDS(nihai_kaydedilecek_liste, file = rds_dosya_adi)

# Bitiş zamanı ve toplam süre
end_time <- Sys.time()
total_time <- end_time - start_time

cat("\n================================================================\n")
cat(" İŞLEM TAMAMLANDI! \n")
if(file.exists(rds_dosya_adi)){
  cat(" Temizlenmiş veri listesi başarıyla şuraya kaydedildi:\n")
  cat(" -> ", normalizePath(rds_dosya_adi), "\n")
  cat(" Quarto dosyanızda artık bu dosyayı okuyabilirsiniz.\n")
} else {
  cat(" -> !! HATA !! Kaydedilmiş dosya bulunamadı! (", rds_dosya_adi, ")\n")
}
cat(" Toplam süre:", format(total_time), "\n")
cat("================================================================\n")




