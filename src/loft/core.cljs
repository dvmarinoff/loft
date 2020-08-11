(ns loft.core
  (:require
   [uix.dom.alpha :as uix.dom]
   [uix.core.alpha :as uix]
   [xframe.core.alpha :as xf :refer [<sub]]

   ["react-slick" :default Slider]
   ))

;; Test REPL Setup
;; (+ 3 4)

(xf/reg-sub :db/primer (fn [] (:primer (xf/<- [::xf/db]))))
(xf/reg-sub :db/base   (fn [] (:base   (xf/<- [::xf/db]))))
(xf/reg-sub :db/nivel  (fn [] (:nivel  (xf/<- [::xf/db]))))

(xf/reg-event-db
 :db/init
 (fn [_ _]
   {:hrefs "http://localhost:8000/"

    :primer {:norm 0.20
             :depth 1
             :area 12
             :size 1
             :price 8.0
             :total-qty 1
             :total-kgs 1
             :total-cost 8.0
             :link
            {:main "https://www.bg.weber/lepila-za-plochki-i-estestveni-kamni/universalni-grundove/weberprim-801"
             :docs ""}
             }

    :base {:norm 2.0
           :depth 10
           :area 12
           :size 25
           :price 4.5
           :total-qty 10
           :total-kgs 240
           :total-cost 45
           :link
           {:main "https://www.bg.weber/podovi-sistemi/podovi-sistemi/weberfloor-basic"
            :docs "https://www.bg.weber/files/bg/2018-02/TDS_weber.floor_Basic_2.pdf"}
           }

    :nivel {:norm 1.5
            :depth 3
            :area 12
            :size 25
            :price 19.5
            :total-qty 3
            :total-kgs 72
            :total-cost 58.5
            :link
            {:main  "https://www.bg.weber/podovi-sistemi/podovi-sistemi/weberfloor-4010"
             :docs "https://www.bg.weber/files/bg/2018-02/TDS_weber.floor_4010_2.pdf"}
            }
    }))

(defn roundp
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn calc-expenditure [norm area depth]
  (* norm area depth))

(defn calc-total-qty [expenditure size]
  (int (Math/ceil (/ expenditure size))))

(defn calc-total-cost [total-qty price]
  (roundp 2 (* total-qty price)))

(defn update-depth [product value]
  (let [s     @product
        v     (int value)
        ntkgs (calc-expenditure (:norm s) (:area s) v)
        ntqty (calc-total-qty ntkgs (:size s))
        ntc   (calc-total-cost ntqty (:price s))]
    (swap! product assoc :depth v)
    (swap! product assoc :total-kgs ntkgs)
    (swap! product assoc :total-qty ntqty)
    (swap! product assoc :total-cost ntc)))

(defn update-area [product value]
  (let [s     @product
        v     (double value)
        ntkgs (calc-expenditure (:norm s) v (:depth s))
        ntqty (calc-total-qty ntkgs (:size s))
        ntc   (calc-total-cost ntqty (:price s))]
    (swap! product assoc :area v)
    (swap! product assoc :total-kgs ntkgs)
    (swap! product assoc :total-qty ntqty)
    (swap! product assoc :total-cost ntc)))

(defn update-price [product value]
  (let [s     @product
        v     (double value)
        ntkgs (calc-expenditure (:norm s) (:area s) (:depth s))
        ntqty (calc-total-qty ntkgs (:size s))
        ntc   (calc-total-cost ntqty v)]
    (swap! product assoc :price v)
    (swap! product assoc :total-kgs ntkgs)
    (swap! product assoc :total-qty ntqty)
    (swap! product assoc :total-cost ntc)))

(defn product-cost [p]
  (let [product (uix/state p)]
      [:div.cost.cf
       [:div.cost-form
        [:div.form-input
         [:label [:span "Thickness (mm): "]
          [:input {:value (:depth @product)
                   :on-change #(update-depth product (.. % -target -value))}]]]

        [:div.form-input
         [:label [:span "Price (bgn/unit): "]
          [:input {:value (clj->js (:price @product))
                   :on-change #(update-price product (.. % -target -value))}]]]

        [:div.form-input
         [:label [:span "Area: (m" [:sup 2] "): "]
          [:input {:value (clj->js (:area @product))
                   :on-change #(update-area product (.. % -target -value))}]]]]

       [:div.results
        [:div.br]
        [:div (clj->js (:norm @product)) " kg/m" [:sup 2] "/1mm"]
        [:div (clj->js (:total-kgs @product)) " kg"]
        [:div (clj->js (:total-qty @product)) " bags"]
        [:div (clj->js (:total-cost @product)) " bgn"]
        ]
       ]))

(defn description []
  (let [images ["images/entrance-light.jpg"
                "images/short-wall-from-chimny.jpg"
                "images/main-from-chimny.jpg"
                "images/main-from-short-corner-light.jpg"
                "images/racks.jpg"
                "images/roof-tall-wall-junction.jpg"
                "images/short-corner.jpg"
                "images/full-view-from-tall-corner.jpg"]
        s (uix/state {:index 0 :isOpen false})
        ]
    [:div.section.cf
     [:h2.h1 "The Project"]
     [:h3.h3 "What do we have?"]

     [:div.text.t1
      [:p "About 12 m" [:sup 2] " of floor space, with angled roof from 2.82 m at the tallest to 1.20 m at the outer wall.
Entrance is located at the East wall and has a chimny of 127x44.5 cm just left of it.
Once you walk past it, a main room of 316x252 cm opens up.
Currently there is no window, but images 4, 5 and 6 show the planned location for one."]]

     [:div.gallery
      [:> Slider
       {:slidesToShow 1
        :adaptiveHeight true
        :dots true}
       [:img.wide {:src "images/floor-plan-1200.png"}]
       [:img.wide {:src "images/south-east-measure-1024.png"}]
       [:img.wide {:src "images/north-west-measure-1024.png"}]
       [:img.wide {:src "images/north-west-window-1024.png"}]
       [:img.wide {:src "images/south-east-window-1200.png"}]
       [:img.wide {:src "images/south-west-window-1024.png"}]
       ]
      ]

     [:div.text.t1
      [:p "And some real pictures:"]]

     [:div.gallery
      [:> Slider
       {:slidesToShow 1
        :adaptiveHeight true
        :dots true}
       [:img.wide {:src "images/full-view-from-tall-corner.jpg"}]
       [:img.tall {:src "images/entrance-light.jpg"}]
       [:img.wide {:src "images/short-wall-from-chimny.jpg"}]
       [:img.wide {:src "images/main-from-chimny.jpg"}]
       [:img.wide {:src "images/main-from-short-corner-light.jpg"}]
       [:img.tall {:src "images/racks.jpg"}]
       [:img.wide {:src "images/roof-tall-wall-junction.jpg"}]
       [:img.wide {:src "images/short-corner.jpg"}]
       ]]
     ])
  )

(defn condition []
  [:div.section.cf.black-bg
   [:h2.h1 "Condition"]
   [:h3.h3 "Before"]
   [:p.text.t1 "Floor condition is most disturbing of all.
It's been heavily used since 1977 while they've never got to installing a finishing surface.
The result is seen in image 1: a broken concrete based substance mixed with all sorts
of small debris left over from the things been stored there."]

[:p.text.t1 "Back then given the material scarcity, quality of construction wasn't up to standart, either.
Image 2 shows the random pieces of brick being thrown in to the mixture.
Now they are forming a number of weak places."]

   [:div.gallery
    [:> Slider
     {:slidesToShow 1
      :adaptiveHeight true
      :dots true}
     [:img.wide {:src "images/condition-floor-before.jpg"}]
     [:img.wide {:src "images/condition-floor-bricks-hd.jpg"}]
     [:img.wide {:src "images/condition-floor-wood.jpg"}]
     [:img.tall {:src "images/condition-floor-after.jpg"}]
     [:img.wide {:src "images/condition-wall-missing-brick.jpg"}]]
    ]
   ])

(defn plan []
  (let [primer (<sub [:db/primer])
        base   (<sub [:db/base])
        nivel  (<sub [:db/nivel])]
    [:div.section.cf
     [:h2.h1 "Plan"]

     [:h3.h2 "1. Finish the floor"]

     [:div.gallery.plan-gallery
      [:> Slider
       {:slidesToShow 1
        :adaptiveHeight true
        :dots true}
       [:img.wide {:src "images/floor-model.png"}]]
      ]

     [:div.steps

      [:div.product
       [:h3.h3 "Step 1: Prime the suface"]
       [:h3.a [:a {:href (-> primer :link :main) :target "_blank"} "Weber Prim 801"]]
       [:img.product-image {:src "images/weber-prim-801.png"}]
       [:p.t1.product-description "Clean the loose material as much as posible and prime the surface with this."]
       [product-cost primer]
       ]

      [:div.product
       [:h3.h3 "Step 2: Laying down a new surface"]
       [:h3.a [:a {:href (-> base :link :main)} "Weber Floor Basic"]]
       [:a {:href (-> base :link :docs) :target "_blank"}
        [:img.product-image {:src "images/weber-floor-basic.png"}]]
       [:p.t1.product-description "Fill the gaps, raise the floor with about 1cm and do some basic leveling of the surface. Layer ~10mm."]
       [product-cost base]
       ]

      [:div.product
       [:h3.h3 "Step 3: Leveling the floor"]
       [:h3.a [:a {:href (-> nivel :link :main)} "Weber Floor 4010"]]
       [:a {:href (-> nivel :link :docs) :target "_blank"}
        [:img.product-image {:src "images/weber-floor-4010.png"}]]
       [:p.t1.product-description "Use this to achieve a pricise levelling of the floor. Layer 3 to 5 mm."]
       [product-cost nivel]
       ]
      ]
     ]))

(defn video-embed [link]
  [:div.video-size
   [:div.video-wrapper
    [:iframe {:width "560"
              :height "315"
              :src link
              :frameborder "0"
              :allow "accelerometer; encrypted-media; gyroscope; picture-in-picture"
              :allowFullScreen 1
              }]]])

(defn video-box [heading link]
  [:div.video-box
   [:h3.h3 heading]
   [video-embed link]
   ])

(defn how-to []
  [:div.section.cf.black-bg
   [:h2.h1 "How to"]

   [:div.row
    [:h3.h2 "Floor"]
    [video-box "Self-levelling Floor" "https://www.youtube.com/embed/TJX-lB64AVI"]]

   [:div.row
    [:h3.h2 "Walls"]
    [:div.video-box
     [video-box "Gypsum Manual Application" "https://www.youtube.com/embed/N-dfv4xysq4"]]
    [:div.video-box
     [video-box "Finish around windows" "https://www.youtube.com/embed/T2-mpo85QZg"]]
    [:div.video-box
     [video-box "Finish with Roller and Knife" "https://www.youtube.com/embed/19DKSlrbVtk"]]
    ]

   [:div.row
    [:h3.h2 "Window"]
    [:p.t1 "how to install a roof window videos are on this "
     [:a {:href "https://www.misiamoiatdom.com/420-%D0%BC%D0%BE%D0%BD%D1%82%D0%B0%D0%B6-%D0%BD%D0%B0-%D0%BF%D0%BE%D0%BA%D1%80%D0%B8%D0%B2%D0%B5%D0%BD-%D0%BF%D1%80%D0%BE%D0%B7%D0%BE%D1%80%D0%B5%D1%86-velux-%D0%BD%D0%BE%D0%B2%D0%BE-%D0%BF%D0%BE%D0%BA%D0%BE%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5"}
      "site"
      ]
     ]
    ]

   [:div.row
    [:h3.h2 "Door"]
    [:p.t1 "We are still a bit early in the planning process, but hopefully will get there soon."]]
   ])

(defn home []
  [:div
   [description]
   [condition]
   [plan]
   [how-to]
   ])

(defn start []
  (prn "start")

  (defonce init-db (xf/dispatch [:db/init]))

  (uix.dom/hydrate
   [home]
   (.getElementById js/document "app")))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds

  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (prn "stop"))

(defn reload [] (prn "reload"))

