(ns conc.macro)

(defn slurpy
  "like slurp, but automaticly search to ./resources
  but also can add path"
  ([link] (slurpy link "./resources/"))
  ([link path] (slurp (str path link ".txt"))))

(defn spity
  "like spit, but automaticly goes to ./resources
  but also can add path"
  ([file input] (spity file "./resources/" input))
  ([file path input] (spit (str path file ".txt") input)))

