
# AKBANK-COBOL-FINALCASE

Bu projede bize verilen input dosyasındaki verileri ilk karakterine göre hangi işlem yapacağimizi ana programda okuyup bu verilere göre vsam dosyamıza alt programda işlem yaptırmamızı istiyor.

Input dosyamız örnek olarak:

w10002521

olsun.
Burdaki ilk karakter hangi işlemi yaptıracağımızı gösteriyor w yazma işlemi geri kalan karakterler ise id yi gösteriyor.

## Write Işlemi:
Burdaki örnekte eğer 10002521 id li bir veri yoksa vsam dosyamızda bu veriyi kendi isim soyisimimiz olarak ekliyoruz fakat aynı id vsam dosyamızda var ise hata dönderiyoruz ve bu hataları yada işlem tamamlandıysa QSAM.HW dosyamızın içine yazdırıyoruz.

## Update Işlemi:

Gelen id deki isim ve soyisimde karakterler arasında boşluk var ise bu boşlukları kaldırıp vsam dosyasına güncelleyip yazıyoruz eğer yok ise hata dönderiyoruz.

## Read Işlemi:

Gelen id eğer vsam dosyasında var ise veriyi okuyup out dosyasına hangi isim soyisime denk geldiğini data kısmında çıktı olarak veriyoruz eğer id yoksa hata veriyoruz.

## Delete Işlemi:

Gelen id eğer vsam dosyasında var ise o veriyi vsam dosyamızdan siliyoruz yok ise hata verdiriyoruz.
