(ns icbl.routes.admin
  (:require [compojure.core :refer :all]
            [clojure.string :as st]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            [noir.io :as io]
            [icbl.models.db :as db]
            [noir.session :as session]
            [clojure.data.json :as json]
            [icbl.routes.teacher :as teacher]
            ))

(defn num-to-str [number]
  (let [snum (str number)]
    (cond
      (<= (count snum) 2) (str snum ",00")
      (= (subs snum 1 2) ".") (if (= (count (subs snum 2 (count snum))) 1) (str (subs snum 0 1) "," (subs snum 2 3) "0")
                                  (str (subs snum 0 1) "," (subs snum 2 (count snum))))
      )))

(defn admin-home []
  (layout/render "admin/home.html")
  )

(defn handle-login [pass]
  (let [vpass (:pass (db/get-data (str "select pass from admin where id='admin'") 1))
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))]
    (if (= vpass pass)
        (do
          (session/put! :id "admin")
          (session/put! :status 1)
          (session/put! :ip ip)
          (layout/render "admin/work.html"))
        (layout/render "admin/home.html" {:error "Password Salah!"}))))

(defn handle-list-nama [nm]
  (let [upnm (clojure.string/upper-case nm)
        data (db/get-data (str "select nis,nama,kelas from users where upper(nama) LIKE '%" upnm "%' order by nama") 2)]
       (if data
         (layout/render "admin/list-siswa-nama.html" {:data data})
         (layout/render "admin/pesan.html" {:pesan "Tidak ada nama tersebut!"}))
    ))

(defn handle-do-edit-siswa [nis]
  (let [datum (db/get-data (str "select * from users where nis='" nis "'") 1)
        daftarkelas (db/get-data "select namakelas from kelas order by namakelas asc" 2)]
    (layout/render "admin/edit-data-siswa.html"
                 {:datum datum :daftarkelas daftarkelas})))

(defn handle-update-data-siswa [nislama nisbaru nama kelas email pass]
  (try (db/update-data-1 "users"
                              ["nis=?" nislama]
                                      {:nis nisbaru
                                       :nama nama
                                       :kelas kelas
                                       :email email
                                       :password pass})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah data siswa!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data siswa!"}))))

(defn handle-delete-data-siswa [nis]
  (try (db/delete-data "users" (str "nis='" nis "'"))
       (layout/render "admin/pesan.html"
                      {:pesan (str "Berhasil menghapus data siswa dengan nis = " nis)})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal menghapus data siswa! error: " ex)}))))

(defn handle-ganti-pw-admin [pwlama pwbaru pwbaru1]
  (let [pwnow (:pass (db/get-data (str "select pass from admin where id='admin'") 1))]
    (if (or (not= pwlama pwnow) (< (count pwbaru) 5))
        (layout/render "admin/pesan.html" {:pesan "Password Lama tidak benar atau password baru kurang dari lima huruf!"})
        (if (= pwbaru pwbaru1)
          (try (spit "data/pw.txt" pwbaru)
                 (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah password admin!"})
               (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))
          (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))))

(defn lihat-guru []
  (let [data (db/get-data (str "select * from teacher order by nama asc") 2)]
    (layout/render "admin/lihat-guru.html" {:data data})))

(defn handle-edit-guru [id]
  (let [datum (db/get-data (str "select * from teacher where id='" id "'") 1)]
    (layout/render "admin/edit-guru.html" {:datum datum})))

(defn handle-update-guru [id nama pass]
  (try
    (db/update-data "teacher" (str "id='" id "'")
       {:nama nama
        :pass pass
        })
    (layout/render "admin/pesan.html" {:pesan (str "Berhasil Update Data Guru!")})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Update Data Guru error: " ex)}))))

(defn daftarkan-guru []
  (layout/render "admin/daftarkan-guru.html"))

(defn handle-daftarkan-guru [id nama]
  (do
    (io/create-path (str "resources/public/proset/" id) true)
    (try
      (db/insert-data "teacher" {:nama nama :id id :pass "abcde"})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil daftarkan Bapak/Ibu " nama " dengan ID " id)})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Daftarkan Guru error: " ex)})))))

(defn admin-pilih-guru [act]
  (let [data (db/get-data (str "select nama,id from teacher order by nama") 2)]
    (layout/render "admin/pilih-guru.html" {:action act :data data})))

(defn admin-set-ip []
  (let [ip (:ipnumber (db/get-data "select ipnumber from ip" 1))]
    (layout/render "admin/change-ip.html" {:ip ip})))

(defn admin-update-ip [ip]
  (try
      (db/update-data-1 "ip" ["no=?" 1] {:ipnumber ip})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil ubah IP Server menjadi " ip)})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Ubah IP Server error: " ex)}))))

(defn logout []
  (do
   (session/clear!)
   (resp/redirect "/admin")))

(defn handle-admin-buat-proset [pel ket jsoal waktu jumpil]
  (try
      (db/insert-data "bankproset"
                               {:id (session/get :id)
                                :pelajaran pel
                                :keterangan ket
                                :jsoal (Integer/parseInt jsoal)
                                :waktu (Integer/parseInt waktu)
                                :kunci (apply str (repeat (Integer/parseInt jsoal) "-"))
                                :jenis (apply str (repeat (Integer/parseInt jsoal) "1"))
                                :upto (apply str (repeat (Integer/parseInt jsoal) "-"))
                                ;:pretext (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                ;:sound (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                :jumpil jumpil
                                :acak "0"
                                :status "0"
                                :skala 10
                                :nbenar 1
                                :nsalah 0})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil daftarkan proset!")})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal daftarkan proset! error: " ex)}))))

(defn handle-admin-search-proset [pel ket act]
  (let [Uket (clojure.string/upper-case ket)
        data (db/get-data (str "select kode,pelajaran,keterangan,jsoal,waktu,status from bankproset where
                               pelajaran='" pel "' and upper(keterangan) LIKE '%" Uket "%'
                               order by keterangan") 2)
        ]
    (layout/render "admin/list-proset.html" {:data data :action act :pel pel :ket ket})))

(defn admin-edit-proset [kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from bankproset where kode='" postkode "'") 1)]
    (layout/render "admin/edit-proset.html" {:datum datum :kode kode})))

(defn admin-update-proset [kode pel ket jsoal waktu jumpil skala nbenar nsalah acak status]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select kunci,jenis,upto,pretext,sound from bankproset where kode='" postkode "'") 1)
        oldkunci (datum :kunci)
        oldjenis (datum :jenis)
        oldupto (datum :upto)
        oldpretext (if (datum :pretext) (read-string (datum :pretext)) nil)
        oldsound (if (datum :sound) (read-string (datum :sound)) nil)
        cok (count oldkunci)
        vjsoal (Integer/parseInt jsoal)
        newkunci (cond
                   (= vjsoal cok) oldkunci
                   (< vjsoal cok) (subs oldkunci 0 vjsoal)
                   :else (str oldkunci (apply str (repeat (- vjsoal cok) "-"))))
        newjenis (cond
                   (= vjsoal cok) oldjenis
                   (< vjsoal cok) (subs oldjenis 0 vjsoal)
                   :else (str oldjenis (apply str (repeat (- vjsoal cok) "1"))))
        newupto (cond
                   (= vjsoal cok) oldupto
                   (< vjsoal cok) (subs oldupto 0 vjsoal)
                   :else (str oldupto (apply str (repeat (- vjsoal cok) "-"))))
        newpretext (if oldpretext
                     (cond
                     (= vjsoal cok) (str oldpretext)
                     (< vjsoal cok) (str (vec (take vjsoal oldpretext)))
                     :else (str (vec (concat oldpretext (repeat (- vjsoal cok) "-"))))) nil)
         newsound (if oldsound
                    (cond
                     (= vjsoal cok) (str oldsound)
                     (< vjsoal cok) (str (vec (take vjsoal oldsound)))
                     :else (str (vec (concat oldsound (repeat (- vjsoal cok) "-"))))) nil)

        ]
  (try
    (db/update-data "bankproset" (str "kode='" postkode "'")
                    {:pelajaran pel :keterangan ket
                     :jsoal vjsoal
                     :waktu (Integer/parseInt waktu)
                     :jumpil jumpil
                     :acak acak
                     :status status
                     :kunci newkunci
                     :jenis newjenis
                     :upto newupto
                     :pretext newpretext
                     :sound newsound
                     :skala (Integer/parseInt skala)
                     :nbenar (Integer/parseInt nbenar)
                     :nsalah (Integer/parseInt nsalah)})
    (layout/render "admin/pesan.html" {:pesan (str "Berhasil update proset!")})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal update proset! error: " ex)})))))

(defn admin-upload-file [kode pel]
  (do
    (io/create-path (str "resources/public/bankproset/" pel "/" kode) true)
    (layout/render "admin/upload.html" {:kode kode :pel pel})))

(defn handle-admin-upload [pel kode file]
  (try
    (if (vector? file)
      (doseq [i file]
          (io/upload-file (str "resources/public/bankproset/" pel "/" kode) i))
      (io/upload-file (str "resources/public/bankproset/" pel "/" kode) file))
      (layout/render "admin/pesan.html" {:pesan "Berhasil upload file!"})
     (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal upload file! error: " ex)}))
    ))


(defn admin-edit-kunci [kode act]
  (let [datum (db/get-data (str "select kunci,jsoal,jenis,upto,pretext,sound from bankproset where kode='" kode"'") 1)]
    (layout/render "teacher/edit-kunci.html" {:kunci (datum :kunci)
                                              :jsoal (datum :jsoal)
                                              :jenis (datum :jenis)
                                              :upto (datum :upto)
                                              :pretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                              :sound (if (datum :sound) (read-string (datum :sound)) nil)
                                              :kode kode
                                              :action act})))

(defn admin-save-kunci [kunci jenis upto pretext sound kode]
  (try
    (db/update-data "bankproset" (str "kode='" kode "'") {:kunci kunci :jenis jenis :upto upto :pretext pretext :sound sound})
    (layout/render "admin/pesan.html" {:pesan "Kunci berhasil disimpan!"})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal simpan kunci! error: " ex)}))))

(defn admin-view-soal [pel kode]
  (let [datum (db/get-data (str "select * from bankproset where kode='" kode "'") 1)]
    (layout/render "admin/view-soal.html" {:datum datum
                                             :nsoal (vec (range 1 (inc (datum :jsoal))))
                                             :kategori "1"
                                             :pel pel
                                             :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                             :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                             ;:soalpath "http://127.0.0.1/resources/public"
                                             })))

(defn admin-lihat-sekaligus [pel kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from bankproset where kode='" postkode "'") 1)]
    (layout/render "admin/view-soal-sekaligus.html" {:datum datum
                                                       :pel pel
                                                       :kode kode
                                                       :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                                       :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                                       ;soalpath "http://localhost/resources/public"
                                                       })))

(defn admin-search-proset [act]
  (let [data (db/get-data "select * from pelajaranbs order by pelajaran" 2)]
    (layout/render "admin/search-proset.html" {:act act :data data})))

(defn admin-confirm-hapus [kode pel ket]
  (let [vkode (subs kode 1 (count kode))
        proset (db/get-data (str "select kode,pelajaran,keterangan from bankproset where kode='" vkode "'") 1)]
    (layout/render "admin/confirm-hapus.html" {:kode kode
                                               :pelajaran (proset :pelajaran)
                                               :keterangan (proset :keterangan)
                                               :pel pel
                                               :ket ket})))

(defn admin-hapus-set [pel ket kode]
  (try
    (db/delete-data "bankproset" (str "kode='" kode "'"))
    (layout/render "admin/pesan.html" {:pesan (str "Set Soal dengan kode B" kode " berhasil dihapus!" )})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal Hapus Proset! error " ex)}))
    ))

(defn handle-admin-tambah-kelas [kls]
  (let [Ukls (clojure.string/upper-case kls)
        data (db/get-data (str "select namakelas from kelas where UPPER(namakelas)='" Ukls "'") 1)]
        (if data
          (layout/render "admin/pesan.html" {:pesan (str "Kelas " Ukls " sudah ada!")})
          (try
            (db/insert-data "kelas" {:namakelas Ukls})
            (layout/render "admin/pesan.html" {:pesan (str "Berhasil menambah kelas dengan nama " Ukls)})
            (catch Exception ex
              (layout/render "admin/pesan.html" {:pesan "Gagal menambah kelas!"}))))))

(defn admin-view-kelas []
  (let [data (db/get-data "select * from kelas order by namakelas" 2)]
    (layout/render "admin/view-kelas.html" {:data data})))

(defn admin-edit-kelas [no]
  (let [datum (db/get-data (str "select nomer,namakelas from kelas where nomer='" no "'") 1)]
    (layout/render "admin/edit-kelas.html" {:datum datum})))

(defn admin-update-kelas [no nklas]
  (try
    (db/update-data "kelas" (str "nomer='" no "'") {:namakelas nklas})
    (layout/render "admin/pesan.html" {:pesan "Berhasil update nama kelas!"})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal update kelas! Error: " ex)}))))

(defn handle-admin-tambah-pelajaran [pel]
  (let [Upel (clojure.string/upper-case pel)
        data (db/get-data (str "select pelajaran from pelajaranbs where UPPER(pelajaran)='" Upel "'") 1)]
        (if data
          (layout/render "admin/pesan.html" {:pesan (str "Kelas " Upel " sudah ada!")})
          (try
            (db/insert-data "pelajaranbs" {:pelajaran Upel})
            (layout/render "admin/pesan.html" {:pesan (str "Berhasil menambah pelajaran dengan nama " Upel)})
            (catch Exception ex
              (layout/render "admin/pesan.html" {:pesan "Gagal menambah pelajaran!"}))))))

(defn admin-view-pelajaran []
  (let [data (db/get-data "select * from pelajaranbs order by pelajaran" 2)]
    (layout/render "admin/view-pelajaran.html" {:data data})))

(defn admin-edit-pelajaran [no]
  (let [datum (db/get-data (str "select nomer,pelajaran from pelajaranbs where nomer='" no "'") 1)]
    (layout/render "admin/edit-pelajaran.html" {:datum datum})))

(defn admin-update-pelajaran [no pel]
  (try
    (db/update-data "pelajaranbs" (str "nomer='" no "'") {:pelajaran pel})
    (layout/render "admin/pesan.html" {:pesan "Berhasil update pelajaran!"})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal update pelajaran! Error: " ex)}))))

(defn handle-input-siswa [file]
  (let [data (slurp (:tempfile file))
        sdata (st/split data #"\n")
        vdata (map #(st/split % #",") (if (not (vector? sdata)) (st/split data #"\r") sdata))]
        (loop [i 0]
          (if (= i (count vdata))
            (layout/render "admin/pesan.html" {:pesan "Menambah data siswa telah selesai!"})
            (do
              (let [nis_ada (db/get-data (str "select nis from users where nis='" ((nth vdata i) 0) "'") 1)]
                (if (not nis_ada)
                  (db/insert-data "users" {:nis ((nth vdata i) 0)
                                           :nama ((nth vdata i) 1)
                                           :kelas ((nth vdata i) 2)
                                           ;:email ((nth vdata i) 3)
                                           ;:NPSN ((nth vdata i) 4)
                                           :password (if ((nth vdata i) 3) ((nth vdata i) 3) "12345")
                                           })))
              (recur (inc i)))))))

(defn admin-delete-siswa [act]
  (let [data (db/get-data "select nis,nama,kelas from users order by kelas,nis" 2)]
    (layout/render "admin/list-all-siswa.html" {:data data :action act :judul "HAPUS SISWA" :ket "menghapus"})))
(defn handle-hapus-siswa [nis]
  (do
    (db/delete-data "users" (str "nis='" nis "'"))
    (admin-delete-siswa "/admin-delete-siswa")))

(defn admin-hapus-guru [act]
  (let [data (db/get-data "select id,nama from teacher order by nama" 2)]
    (layout/render "admin/list-all-guru.html" {:data data :action act :judul "HAPUS GURU" :ket "menghapus"})))
(defn handle-hapus-guru [id]
  (do
    (db/delete-data "teacher" (str "id='" id "'"))
    (admin-hapus-guru "/admin-hapus-guru")))

;;;routes
(defroutes admin-routes

  (GET "/admin" []
      (admin-home))

  (GET "/admin-home" []
       (layout/render "admin/work.html"))

  (GET "/admin-logout" []
       (logout))

  (POST "/admin-login" [pass]
      (handle-login pass))

  (GET "/edit-siswa" []
       (layout/render "admin/search-siswa.html"))
  (POST "/edit-siswa" [nama]
        (handle-list-nama nama))
  (POST "/do-edit-siswa" [nis]
        (handle-do-edit-siswa nis))
  (POST "/update-data-siswa" [nislama nisbaru nama kelas email pass]
        (handle-update-data-siswa nislama nisbaru nama kelas email pass))
  (POST "/delete-data-siswa" [nislama]
        (handle-delete-data-siswa nislama))
  (GET "/admin-delete-siswa" []
       (admin-delete-siswa "/admin-delete-siswa"))
  (POST "/admin-delete-siswa" [nis]
        (handle-hapus-siswa nis))

  (GET "/admin-tambah-kelas" []
       (layout/render "admin/tambah-kelas.html"))
  (POST "/admin-tambah-kelas" [kelas]
        (handle-admin-tambah-kelas kelas))

  (GET "/admin-edit-kelas" []
       (admin-view-kelas))
  (POST "/admin-edit-kelas" [nomer]
      (admin-edit-kelas nomer))
  (POST "/admin-update-kelas" [nomer namakelas]
      (admin-update-kelas nomer namakelas))

  (GET "/admin-tambah-pelajaran" []
       (layout/render "admin/tambah-pelajaran.html"))
  (POST "/admin-tambah-pelajaran" [pelajaran]
        (handle-admin-tambah-pelajaran pelajaran))

  (GET "/admin-edit-pelajaran" []
       (admin-view-pelajaran))
  (POST "/admin-edit-pelajaran" [nomer]
      (admin-edit-pelajaran nomer))
  (POST "/admin-update-pelajaran" [nomer pelajaran]
      (admin-update-pelajaran nomer pelajaran))

  (GET "/ganti-pw-admin" []
       (layout/render "admin/ganti-pw-admin.html"))
  (POST "/ganti-pw-admin" [pwlama pwbaru pwbaru1]
        (handle-ganti-pw-admin pwlama pwbaru pwbaru1))

  (GET "/lihat-guru" []
       (lihat-guru))
  (POST "/edit-guru" [id]
        (handle-edit-guru id))
  (POST "/update-guru" [id nama pass]
        (handle-update-guru id nama pass))
  (GET "/admin-hapus-guru" []
       (admin-hapus-guru "/admin-hapus-guru"))
  (POST "/admin-hapus-guru" [id]
        (handle-hapus-guru id))

  (GET "/daftarkan-guru" []
       (daftarkan-guru))
  (POST "/daftarkan-guru" [id nama]
        (handle-daftarkan-guru id nama))

  (GET "/admin-hasil-testL" []
       (admin-pilih-guru "/admin-pilih-proset"))
  (POST "/admin-pilih-proset" [id]
        (teacher/teacher-pilih-proset "L" id "/teacher-pilih-kelas"))
  (GET "/admin-hasil-testB" []
       (admin-search-proset "/admin-hasil-test-search"))
  (POST "/admin-hasil-test-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-pilih-kelasB"))
  (POST "/admin-pilih-kelasB" [kode]
        (teacher/teacher-pilih-kelas kode "/admin-hasil-testB"))
  (POST "/admin-hasil-testB" [kode kelas]
       (teacher/teacher-hasil-test kode kelas "teacher/hasil-test.html"))

  ;;Analisis Butir Soal
  (GET "/admin-abs" []
        (admin-pilih-guru "/admin-pilih-proset-absbsk"))
  (POST "/admin-pilih-proset-absbsk" [id]
        (teacher/teacher-pilih-proset "L" id "/teacher-abs"))

  (GET "/admin-abs-tk" []
       (admin-pilih-guru "/admin-pilih-proset-abstk"))
  (POST "/admin-pilih-proset-abstk" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-tk"))

  (GET "/admin-abs-dp" []
       (admin-pilih-guru "/admin-pilih-proset-absdp"))
  (POST "/admin-pilih-proset-absdp" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-dp"))

  (GET "/admin-dayakecoh" []
       (admin-pilih-guru "/admin-pilih-proset-absdk"))
   (POST "/admin-pilih-proset-absdk" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-dayakecoh"))

  (GET "/admin-absB" []
       (admin-search-proset "/admin-absB-search"))
  (POST "/admin-absB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-absB"))
  (POST "/admin-absB" [kode]
        (teacher/teacher-abs kode "teacher/hasil-abs.html"))

  (GET "/admin-abs-tkB" []
       (admin-search-proset "/admin-abs-tkB-search"))
  (POST "/admin-abs-tkB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-abs-tkB"))
  (POST "/admin-abs-tkB" [kode]
        (teacher/teacher-abs-tk kode "teacher/hasil-abs-tk.html"))

  (GET "/admin-abs-dpB" []
       (admin-search-proset "/admin-abs-dpB-search"))
  (POST "/admin-abs-dpB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-abs-dpB"))
  (POST "/admin-abs-dpB" [kode]
        (teacher/teacher-abs-dp kode "teacher/hasil-abs-dp.html"))

  (GET "/admin-dayakecohB" []
       (admin-search-proset "/admin-dayakecohB-search"))
  (POST "/admin-dayakecohB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-dayakecohB"))
  (POST "/admin-dayakecohB" [kode]
        (teacher/teacher-dayakecoh kode "teacher/hasil-dayakecoh.html"))

  ;;Simpan ke Excel

  (GET "/admin-hasil-test-excel" []
       (admin-pilih-guru "/admin-pilih-proset-excel"))
  (POST "/admin-pilih-proset-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-pilih-kelas-excel"))

  (GET "/admin-abs-excel" []
        (admin-pilih-guru "/admin-pilih-proset-abs-excel"))
   (POST "/admin-pilih-proset-abs-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-excel"))

  (GET "/admin-abs-tk-excel" []
        (admin-pilih-guru "/admin-pilih-proset-abstk-excel"))
   (POST "/admin-pilih-proset-abstk-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-tk-excel"))

  (GET "/admin-abs-dp-excel" []
        (admin-pilih-guru "/admin-pilih-proset-absdp-excel"))
   (POST "/admin-pilih-proset-absdp-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-dp-excel"))

  (GET "/admin-adk-excel" []
        (admin-pilih-guru "/admin-pilih-proset-adk-excel"))
   (POST "/admin-pilih-proset-adk-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-adk-excel"))

  (GET "/admin-hasil-test-excelB" []
       (admin-search-proset "/admin-hasil-test-excelB-search"))
  (POST "/admin-hasil-test-excelB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-pilih-kelas-excelB"))
  (POST "/admin-pilih-kelas-excelB" [kode]
        (teacher/teacher-pilih-kelas kode "/admin-hasil-test-excelB"))
  (POST "/admin-hasil-test-excelB" [kode kelas]
        (teacher/teacher-hasil-test kode kelas "teacher/hasil-test-excel.html"))

  (GET "/admin-abs-excelB" []
       (admin-search-proset "/admin-abs-excelB-search"))
  (POST "/admin-abs-excelB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-abs-excelB"))
  (POST "/admin-abs-excelB" [kode]
        (teacher/teacher-abs kode "teacher/hasil-abs-excel.html"))

  (GET "/admin-abs-tk-excelB" []
       (admin-search-proset "/admin-abs-tk-excelB-search"))
  (POST "/admin-abs-tk-excelB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-abs-tk-excelB"))
  (POST "/admin-abs-tk-excelB" [kode]
        (teacher/teacher-abs-tk kode "teacher/hasil-abs-tk-excel.html"))

  (GET "/admin-abs-dp-excelB" []
       (admin-search-proset "/admin-abs-dp-excelB-search"))
  (POST "/admin-abs-dp-excelB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-abs-dp-excelB"))
  (POST "/admin-abs-dp-excelB" [kode]
        (teacher/teacher-abs-dp kode "teacher/hasil-abs-dp-excel.html"))

  (GET "/admin-adk-excelB" []
       (admin-search-proset "/admin-adk-excelB-search"))
  (POST "/admin-adk-excelB-search" [pel ket]
       (handle-admin-search-proset pel ket "/admin-adk-excelB"))
  (POST "/admin-adk-excelB" [kode]
        (teacher/teacher-dayakecoh kode "teacher/hasil-adk-excel.html"))

  (GET "/admin-set-ip" []
       (admin-set-ip))
  (POST "/admin-change-ip" [ipnumber]
        (admin-update-ip ipnumber))

  (GET "/admin-buat-proset" []
       (let [data (db/get-data  "select pelajaran from pelajaranbs order by pelajaran" 2)]
         (layout/render "admin/buat-proset.html" {:data data})))
  (POST "/admin-buat-proset" [pel ket jsoal waktu jumpil]
        (handle-admin-buat-proset pel ket jsoal waktu jumpil))

  (GET "/admin-search-proset" []
       (admin-search-proset "/admin-search-proset1"))
  (POST "/admin-search-proset1" [pel ket]
        (handle-admin-search-proset pel ket "/admin-edit-proset"))
  (POST "/admin-edit-proset" [kode]
        (admin-edit-proset kode))
  (POST "/admin-update-proset" [kode pel ket jsoal waktu jumpil skala nbenar nsalah acak status]
         (admin-update-proset kode pel ket jsoal waktu jumpil skala nbenar nsalah acak status))

  (GET "/admin-upload-file" []
       (admin-search-proset "/admin-pilih-proset1"))
  (POST "/admin-pilih-proset1" [pel ket]
       (handle-admin-search-proset pel ket "/admin-upload-file1"))
  (POST "/admin-upload-file1" [kode pel]
        (admin-upload-file (subs kode 1 (count kode)) pel))
  (POST "/admin-upload" [pel kode file]
        (handle-admin-upload pel kode file))

  (GET "/admin-edit-kunci" []
       (admin-search-proset "/admin-edit-kunci-search"))
  (POST "/admin-edit-kunci-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-edit-kunci1"))
  (POST "/admin-edit-kunci1" [kode]
        (admin-edit-kunci (subs kode 1 (count kode)) "/admin-save-kunci"))
  (POST "/admin-save-kunci" [kunci jenis upto pretext sound kode]
        (admin-save-kunci kunci jenis upto (str "[" pretext "]") (str "[" sound "]") kode))

  (GET "/admin-lihat-soal" []
       (admin-search-proset "/admin-lihat-soal-search"))
  (POST "/admin-lihat-soal-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-lihat-soal1"))
  (POST "/admin-lihat-soal1" [pel kode]
        (admin-view-soal pel (subs kode 1 (count kode))))

  (GET "/admin-lihat-sekaligus" []
       (admin-search-proset "/admin-lihat-sekaligus-search"))
  (POST "/admin-lihat-sekaligus-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-lihat-sekaligus1"))
  (POST "/admin-lihat-sekaligus1" [pel kode]
        (admin-lihat-sekaligus pel kode))

  (GET "/admin-hapus-set" []
       (admin-search-proset "/admin-hapus-set-search"))
  (POST "/admin-hapus-set-search" [pel ket]
      (handle-admin-search-proset pel ket "/admin-confirm-hapus"))
  (POST "/admin-confirm-hapus" [kode pel ket]
        (admin-confirm-hapus kode pel ket))
  (POST "/admin-confirm-fback" [kode pel ket yn]
        (if (= yn "Y") (admin-hapus-set pel ket (subs kode 1 (count kode))) (layout/render "admin/work.html")))
  (POST "/admin-hapus-set1" [pel ket kode]
        (admin-hapus-set pel ket (subs kode 1 (count kode))))

  (GET "/admin-input-siswa" []
       (layout/render "admin/input-siswa.html"))
  (POST "/admin-input-siswa" [file]
        (handle-input-siswa file))
)
