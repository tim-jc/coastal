ferries <- dplyr::tribble(
  ~ferry, ~activity_id, ~lat, ~lng,
  "Tilbury -> Gravesend", 5906287061, 51.448386, 0.371552,
  "Sandbanks", 5575822827, 50.681738, -1.949315,
  "Dartmouth -> Kingswear", 5564763338, 50.348742, -3.575669,
  "Salcombe", 5564763338, 50.235731, -3.765182,
  "Torpoint -> Plymouth", 5564763338, 50.375810, -4.188294,
  "Fowey -> Polruan", 5560406484, 50.331343, -4.635739,
  "Falmouth -> St Mawes", 5560406484, 50.154367, -5.036136,
  "Padstow", 5824943588, 50.543438, -4.930992,
  "Ardrossan -> Brodick", 7055062883, 55.607871, -4.969103,
  "Lochranza -> Claonaig", 7060605792, 55.733197, -5.339097,
  "Oban -> Craignure", 7072272567, 56.440869, -5.583040,
  "Tobermory -> Kilchoan", 7072272567, 56.658632, -6.066250,
  "Mallaig -> Armadale", 7076591578, 57.039403, -5.852108,
  "East Cowes -> West Cowes", 7403080498, 50.757738, -1.291578,
  "Hayling Island", 8848979854, 50.796715, -1.027668,
  "Portsmouth -> Gosport", 8848979854, 50.795424, -1.112938,
  "Warsash -> Hamble", 8848979854, 50.855839, -1.310089,
  "Mudeford", 8848979854, 50.721583, -1.743585,
  "Ferryside -> Llansteffan", 8978887674, 51.770486, -4.377032,
  "Butley", 9634786328, 52.080330, 1.490391,
  "Bawdsey -> Felixstowe", 9634786328, 51.990132, 1.392646,
  "Felixstowe -> Harwich", 9634786328, 51.945829, 1.304259,
  "Brightlingsea -> Mersea Island", 9634786328, 51.800945, 1.011537,
  "Burnham YH -> Essex Marina", 9641255722, 51.623854, 0.803652,
  "Itchenor", 15095197221, 50.808844, -0.865772,
  "Ardmhòr -> Eriskay", 15720148185, 57.045633, -7.361339,
  "Berneray -> Leverburgh", 15720148185, 57.710011, -7.045988
) %>%
  dplyr::mutate(
    ferry = stringr::str_glue("{ferry} Ferry"),
    activity_id = bit64::as.integer64(activity_id)
  )
