(ns icbl.models.updatedb
    (:require [clojure.java.jdbc :as sql]
              [icbl.models.db :as db]))

(def db1
{:subprotocol "postgresql"
:subname "//localhost:5432/tosmafat"
:user "tosma"
:password "tosma2000"})

(defmacro with-db1 [f & body]
`(sql/with-connection ~db1 (~f ~@body)))

(defn get-data1 [query toggle]
  (if (= toggle 1)
  (with-db1 sql/with-query-results res [query] (first res))
  (with-db1 sql/with-query-results res [query] (doall res))))

(defn update [id pelajaran]
  (let [datafat (get-data1 (str "select * from proset where id='" id "'") 2)
        ;maxkode (db/get-data (str "select MAX(kode) from bankproset where pelajaran='" pelajaran "'") 1)
        ;mkd (if (= (:max maxkode) nil) 0 (:max maxkode))
        ;datatambah (filter #(> (:kode %) mkd) datafat)
        datatambah datafat
        cdt (count datatambah)
       ]
    (loop [i 0]
      (if (= i cdt)
        "selesai"
        (do
          (db/insert-data "bankproset" {:kode (:kode (nth datatambah i))
                                        :jsoal (:jsoal (nth datatambah i))
                                        :waktu (:waktu (nth datatambah i))
                                        :pelajaran pelajaran
                                        :keterangan (str (:pelajaran (nth datatambah i)) " " (:keterangan (nth datatambah i)))
                                        :status (:status (nth datatambah i))
                                        :id id
                                        :kunci (:kunci (nth datatambah i))
                                        :jenis (:jenis (nth datatambah i))
                                        :upto (:upto (nth datatambah i))
                                        :acak (:acak (nth datatambah i))
                                        :skala 10
                                        :nbenar 1
                                        :nsalah 0
                                        })
          (recur (inc i))))
    )
    )
  )

(defn modify-ps []
  (let [data (db/get-data (str "select jsoal,kode from bankproset") 2)]
    (doseq [x data] (db/update-data-1 "bankproset" ["kode=?" (:kode x)] {:pretext (str (vec (repeat (:jsoal x) "-")))
                                                                         :sound (str (vec (repeat (:jsoal x) "-")))}))))
