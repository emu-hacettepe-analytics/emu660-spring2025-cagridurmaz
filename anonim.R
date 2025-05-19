# ================================================================
# VERI HAZIRLAMA BETİĞİ (veri_hazirla.R) - ANONİMLEŞTİRME EKLENDİ
# ================================================================
# Amaç: Ham CSV dosyalarını okumak, ID'leri standartlaştırmak,
#       sahte ID'ler oluşturmak, test kullanıcılarını silmek,
#       ORJİNAL STANDART ID'Yİ KALDIRMAK ve sonuçları RDS olarak kaydetmek.
#
# Çalıştırma: Bu betiği R veya RStudio'da bir kere çalıştırın.
#             Sonuç olarak bu betiğin bulunduğu klasör içinde
#             "islenmis_veri" adlı bir klasör ve onun içinde de
#             "anonim_temiz_veri_listesi.rds" dosyası oluşacaktır.
# ================================================================

# Başlangıç zamanı
start_time <- Sys.time()

# --- Gerekli Kütüphaneler ---
cat("Gerekli kütüphaneler yükleniyor...\n")
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
standard_id_sutun_adi <- "standardize_number" # Kaldırılacak sütun
sahte_id_sutun_adi <- "fake_student_id"      # Kalacak sütun
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
  veri_listesi <- compact(veri_listesi)
  cat(length(veri_listesi), "adet CSV dosyası başarıyla okundu.\n")
}

# --- 3. Yeniden Adlandırma Fonksiyonu ---
rename_student_id_column <- function(df, hedef_sutun_adi){
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
  if (!is.data.frame(df)) return(df)
  # ÖNEMLİ: Sahte ID'yi eklemeden ÖNCE orijinal ID sütununun varlığından emin olmalıyız.
  if (original_col %in% names(df) && !is.null(mapping_vector) && length(mapping_vector) > 0) {
    # Yeni sütun zaten varsa dokunma
    if (new_col %in% names(df)) return(df)
    # Sahte ID'yi ekle
    df <- df %>% mutate(!!new_col := mapping_vector[ .data[[original_col]] ])
  } else if (!is.null(mapping_vector) && length(mapping_vector) > 0) {
    # Eğer orijinal ID sütunu yoksa ama map varsa, belki başka bir ID sütunu vardır?
    # Bu durum için özel bir işlem gerekebilir, şimdilik atlıyoruz veya uyarı verilebilir.
    # warning("Orijinal ID sütunu ('", original_col, "') bulunamadı, sahte ID eklenemedi.")
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
kaynak_liste_test_silme_icin <- veri_listesi_final # Bir önceki adımdan gelen liste
yeni_liste_test_silinmis_adi <- "veri_listesi_test_silinmis"
id_sutunu_adi_silme <- standard_id_sutun_adi # Test ID'leri hala standart ID üzerinden kontrol ediliyor
users_tablo_adi <- "users"

if (!exists("kaynak_liste_test_silme_icin") || !is.list(kaynak_liste_test_silme_icin)) {
  stop("HATA: Test silme için kaynak liste ('kaynak_liste_test_silme_icin') bulunamadı.")
}
veri_listesi_test_silinmis <- kaynak_liste_test_silme_icin # Başlangıçta kopyala

if (!(users_tablo_adi %in% names(kaynak_liste_test_silme_icin))) {
  warning("UYARI: '", users_tablo_adi, "' tablosu bulunamadı, test kullanıcısı silme işlemi atlanıyor.")
} else {
  users_df_silme <- kaynak_liste_test_silme_icin[[users_tablo_adi]]
  if (!is.data.frame(users_df_silme)) {
    warning("UYARI: '", users_tablo_adi, "' bir veri çerçevesi değil, test kullanıcısı silme işlemi atlanıyor.")
  } else if (!(id_sutunu_adi_silme %in% names(users_df_silme))) {
    warning("UYARI: '", id_sutunu_adi_silme, "' sütunu (test kontrolü için) '", users_tablo_adi, "' tablosunda yok, silme işlemi atlanıyor.")
  } else {
    cat("ID'si 'test' ile başlayan kullanıcılar tespit ediliyor ('", users_tablo_adi, "' tablosundan)...\n", sep="")
    silinecek_test_idler <- users_df_silme %>%
      filter(!is.na(.data[[id_sutunu_adi_silme]]) & startsWith(as.character(.data[[id_sutunu_adi_silme]]), "test")) %>%
      pull(.data[[id_sutunu_adi_silme]]) %>%
      unique()
    
    if (length(silinecek_test_idler) == 0) {
      cat("ID'si 'test' ile başlayan ve silinecek kayıt bulunamadı.\n")
    } else {
      cat(length(silinecek_test_idler), " adet benzersiz 'test' ID'sine sahip kullanıcının tüm tablolardaki kayıtları silinecek.\n")
      # ÖNEMLİ: Silme işlemi hem standart ID hem de sahte ID üzerinden yapılmalı mı?
      # Eğer sahte ID eklendikten sonra standart ID silinecekse, silme işlemi
      # standart ID hala varken yapılmalı. Mevcut kod bu şekilde çalışıyor.
      veri_listesi_test_silinmis <- map(kaynak_liste_test_silme_icin, function(df) {
        # Standart ID'ye göre filtrele
        if (is.data.frame(df) && (id_sutunu_adi_silme %in% names(df))) {
          df <- df %>% filter(!(.data[[id_sutunu_adi_silme]] %in% silinecek_test_idler))
        }
        # Not: Eğer sahte ID de varsa ve ona göre de silmek gerekirse, ek bir filtre adımı gerekir.
        # Şimdilik sadece standart ID'ye göre siliniyor.
        return(df)
      })
      cat("\nSilme işlemi tamamlandı. Temizlenmiş veriler '", yeni_liste_test_silinmis_adi, "' listesinde.\n", sep="")
    }
  }
}

# --- 10. YENİ ADIM: Orjinal Standart ID Sütununu Kaldırma ---
cat("\n--- 10. Orjinal Standart ID Sütunu ('", standard_id_sutun_adi, "') Kaldırılıyor ---\n", sep="")

kaynak_liste_anonim_icin <- veri_listesi_test_silinmis # Bir önceki adımdan gelen liste
yeni_liste_anonim_adi <- "veri_listesi_anonim"

# Kaldırma fonksiyonu
remove_standardized_id <- function(df, col_to_remove) {
  if (is.data.frame(df) && col_to_remove %in% names(df)) {
    # select fonksiyonu ile belirtilen sütunu çıkar
    # all_of() kullanımı, col_to_remove değişkenindeki ismi güvenle kullanmayı sağlar
    df <- df %>% select(-all_of(col_to_remove))
  }
  return(df)
}

# Fonksiyonu listedeki her dataframe'e uygula
veri_listesi_anonim <- map(kaynak_liste_anonim_icin,
                           ~remove_standardized_id(.x, standard_id_sutun_adi))

# İsteğe bağlı: Kaç veri setinden sütunun kaldırıldığını kontrol etme
# sutun_kaldirildi_sayisi <- sum(map_lgl(names(veri_listesi_anonim),
#                               ~ standard_id_sutun_adi %in% names(veri_listesi_anonim[[.x]])))
# cat(length(veri_listesi_anonim) - sutun_kaldirildi_sayisi, "adet veri setinden '", standard_id_sutun_adi, "' sütunu kaldırıldı (veya hiç yoktu).\n")
cat("Orjinal standart ID sütununun kaldırılma işlemi tamamlandı.\n")


# --- 11. İşlenmiş ve Anonimleştirilmiş Veriyi Kaydetme (RDS olarak) ---
# Not: Önceki numaralandırmada 10. adımdı.
cat("\n--- 11. İşlenmiş ve Anonimleştirilmiş Veri Kaydediliyor ---\n")

# Kaydedilecek nihai listenin adı (artık anonim liste)
nihai_kaydedilecek_liste_adi <- "veri_listesi_anonim" # <<< Değişiklik

# Bu listenin var olduğundan emin olalım
if (!exists(nihai_kaydedilecek_liste_adi)) {
  stop("HATA: Kaydedilecek '", nihai_kaydedilecek_liste_adi, "' listesi oluşturulamadı!")
}
nihai_kaydedilecek_liste <- get(nihai_kaydedilecek_liste_adi)

# Kaydedilecek klasör ve dosya adını TANIMLA
kayit_klasoru <- "islenmis_veri"
# Dosya adını da güncelleyelim
rds_dosya_adi <- file.path(kayit_klasoru, "anonim_temiz_veri_listesi.rds") # <<< Değişiklik
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
if (exists("nihai_kaydedilecek_liste") && exists("rds_dosya_adi")) {
  cat("Kaydediliyor...\n")
  saveRDS(nihai_kaydedilecek_liste, file = rds_dosya_adi)
  
  # Bitiş zamanı ve toplam süre
  end_time <- Sys.time()
  total_time <- end_time - start_time
  
  cat("\n================================================================\n")
  cat(" İŞLEM TAMAMLANDI! \n")
  if(file.exists(rds_dosya_adi)){
    cat(" Temizlenmiş ve anonimleştirilmiş veri listesi başarıyla şuraya kaydedildi:\n") # <<< Mesaj güncellendi
    cat(" -> ", normalizePath(rds_dosya_adi), "\n")
    cat(" Quarto dosyanızda artık bu dosyayı okuyabilirsiniz.\n")
  } else {
    cat(" -> !! HATA !! Kaydedilmiş dosya bulunamadı! (", rds_dosya_adi, ")\n")
  }
  cat(" Toplam süre:", format(total_time), "\n")
  cat("================================================================\n")
  
} else {
  stop("HATA: Kaydedilecek veri veya dosya yolu bulunamadı!")
}