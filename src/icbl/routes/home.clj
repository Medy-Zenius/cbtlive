(ns icbl.routes.home
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            ;[noir.validation :as vali]
            ;[noir.util.crypt :as crypt]
            [noir.response :as resp]
            [icbl.models.db :as db]
            [noir.session :as session]
            [icbl.models.share :as share]
            ))

(defn handle-login [nis pass]
  (let [user (db/get-data (str "select nis,password,nama from users where nis='" nis "'") 1)
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))
        ]
      (if user
         (if (= pass (user :password))
           (do
             (session/put! :id nis)
             (session/put! :nama (user :nama))
             (session/put! :ip ip)
             (layout/render "home/home.html"))
           (layout/render "share/login.html"
                          {:error "Password Salah!" :nis nis}))
         (layout/render "share/login.html"
                          {:error "Tidak ada user dengan NIS tersebut!"}))
    ))

(defn acak-soal
  [dt]
  (let [a (group-by #(nth % 3) dt)
        b (filter #(= "-" (first %)) a)
        b1 (group-by #(last %) (second (first b)))
        b2 (filter #(= "-" (first %)) b1)
        b3 (map #(second %) b2)

        c (filter #(not= "-" (first %)) b1)
        c1 (map #(second %) c)

        d (filter #(not= "-" (first %)) a)
        e (map #(second %) d)
        f (concat (first b3) c1 e)
        g (shuffle f)
        h (partition 5 (flatten g))
        ]
    h))

(defn handle-kodeto1 [kodeto]
  (let [pre (subs kodeto 0 1)
        kd (subs kodeto 1 (count kodeto))]
     (if (and (not= pre "B") (not= pre "L"))
       (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto})
       (let [data (if (= pre "B")
                      (db/get-data (str "select * from bankproset where kode='" kd "'") 1)
                      (db/get-data (str "select * from proset where kode='" kd "'") 1))]

         (if (and data (= (data :status) "1"))
           (let [jsoal (data :jsoal)
                 vjaw (partition 5 (interleave (range 1 (inc jsoal)) (data :jenis) (data :upto)
                                               (if (data :pretext) (read-string (data :pretext)) (repeat jsoal "-"))
                                               (if (data :pretext) (read-string (data :sound)) (repeat jsoal "-"))))
                 ;vjaw-acak vjaw
                 vjaw1 (if (= "1" (data :acak)) (acak-soal vjaw) vjaw)
                 nsoal (vec (map #(first %) vjaw1))
                 njenis (vec (map #(second %) vjaw1))
                 nupto (apply str (map #(str (nth % 2)) vjaw1))
                 npretext (vec (map #(nth % 3) vjaw1))
                 nsound (vec (map #(last %) vjaw1))
                 ;;page (if (= pre "B") "home/tryoutB.html" "home/tryout.html")
                 page "home/tryout.html"
                 ]
                ;(println nupto)
                (layout/render page {:data data
                                     :nsoal nsoal
                                     :njenis njenis
                                     :nupto nupto
                                     :npretext npretext
                                     :nsound nsound
                                     :kodeto kodeto}))
           (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto}))
    ))))


(defn handle-simpan-jawaban [kode jawaban ni]
  (let [nis ni
        prekode (subs kode 0 1)
        remkode (subs kode 1 (count kode))
        tdata (if (= (subs kode 0 1) "B") "bankproset" "proset")
        dproset (db/get-data (str "select * from " tdata " where kode='" remkode "'") 1)
        ada (db/get-data (str "select nis from dataus where nis='" nis "' and kode='" kode "'") 1)

        jsoal (count jawaban)
        kunci (vec (map str (seq (:kunci dproset))))

        jbenar (loop [jb 0, i 0]
                          (if (= i jsoal)
                              jb
                              (recur (if (= (subs jawaban i (inc i)) (kunci i)) (inc jb) jb) (inc i))))
        jkosong (count (filter #(= % \-) (vec jawaban)))
        jsalah (- jsoal (+ jbenar jkosong))
        skala (:skala dproset)
        nbenar (:nbenar dproset)
        nsalah (:nsalah dproset)
        nilai (/ (Math/round (* (/ (+ (* jbenar nbenar) (* jsalah nsalah)) (* jsoal nbenar)) skala 100.0)) 100.0)
        vkd kode
        ]
         (if (not ada)
             (try (db/insert-data "dataus"  {:nis nis
                                             :kode vkd
                                             :jawaban jawaban
                                             :nilai nilai
                                             :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
              {:nilai nilai}
               ;{:nilai nil}
              (catch Exception ex
                {:nilai nil}))
             (try (db/update-data-1 "dataus"
                                    ["nis=? AND kode=?" nis vkd]
                                      {:nis nis
                                       :kode vkd
                                       :jawaban jawaban
                                       :nilai nilai
                                       :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
               {:nilai nilai}
               (catch Exception ex
                {:nilai nil}))
           )))

(defn home-login []
  (layout/render "share/login.html"))

(defn home []
  (layout/render "home/home.html"))

(defn home-registrasi-siswa []
  (let [daftarkelas (db/get-data "select namakelas from kelas order by namakelas asc" 2)
        kelas (:namakelas (first daftarkelas))]
    (layout/render "share/registrasi-siswa.html" {:daftarkelas daftarkelas :kelas kelas})))

(defn handle-reg-siswa [nis nama kelas email pw1 pw2]
  (let [user (db/get-data (str "select nis from users where nis='" nis "'") 1)
        daftarkelas (db/get-data "select namakelas from kelas order by namakelas asc" 2)]
    (if user
      (layout/render "share/registrasi-siswa.html"
                     {:error "NIS tersebut sudah terdaftar!"
                      :nis nis :vnama nama :kelas kelas :email email :daftarkelas daftarkelas})
      (if (not= pw1 pw2)
          (layout/render "share/registrasi-siswa.html"
                         {:error "Kata Sandi tidak cocok!"
                          :nis nis :vnama nama :kelas kelas :email email :daftarkelas daftarkelas})
          (if (< (count pw1) 5)
              (layout/render "share/registrasi-siswa.html"
                             {:error "Kata sandi paling sedikit 5 digit!"
                              :nis nis :vnama nama :kelas kelas :email email :daftarkelas daftarkelas})
              (do
                (db/insert-data "users" {:nis nis :kelas kelas :email email :password pw1 :nama nama})
                (session/put! :id nis)
                (session/put! :nama nama)
                (layout/render "share/login.html"))))
      )))

(defn handle-to-lanjutan [kode]
  (let [pre (subs kode 0 1)
        remko (subs kode 1 (count kode))
        tabel (if (= pre "B") "bankproset" "proset")
        data (db/get-data (str "select pelajaran,keterangan from " tabel " where kode='" remko "'") 1)]
  (layout/render "home/tryout-lanjutan.html" {:data data :kodeto kode})))

(defroutes home-routes
  (GET "/" [] (home-login))
  (GET "/home" []
       (home))
  (POST "/home-login" [nis pass]
       (handle-login nis pass))

  (GET "/registrasi-siswa" []
       (home-registrasi-siswa))
  (POST "/registrasi-siswa" [nis nama kelas email pass1 pass2]
        (handle-reg-siswa nis nama kelas email pass1 pass2))

  (GET "/home-logout" []
       (share/logout "/"))

  (POST "/home-no-lstore" []
        (layout/render "home/kode1.html"))

  (POST "/home-lstore" []
        (layout/render "home/kode2.html"))

  (POST "/home-kodeto" [kodeto]
        (handle-kodeto1 kodeto))

    (POST "/home-tryout-lanjutan" [kode]
        (handle-to-lanjutan kode))

  (POST "/home-tryout-baru" []
        (layout/render "home/kode1.html"))

  (GET "/simpan/:kode/:jawaban/:nis" [kode jawaban nis]
       ;(println (str kode " " jawaban))
      (resp/json (handle-simpan-jawaban kode jawaban nis)))

)
