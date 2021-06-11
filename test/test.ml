open OUnit2
open Gen_yojson.Gen

let json =
  {|{
  "coord": {
    "lon": 14.42,
    "lat": 50.09
  },
  "weather": [
    {
      "id": 802,
      "main": "Clouds",
      "description": "scattered clouds",
      "icon": "03d"
    }
  ],
  "base": "cmc stations",
  "main": {
    "temp": 5,
    "pressure": 1010,
    "humidity": 100,
    "temp_min": 5,
    "temp_max": 5
  },
  "wind": { "speed": 1.5, "deg": 150 },
  "clouds": { "all": 32 },
  "dt": 1460700000,
  "sys": {
    "type": 1,
    "id": 5889,
    "message": 0.0033,
    "country": "CZ",
    "sunrise": 1460693287,
    "sunset": 1460743037
  },
  "id": 3067696,
  "name": "Prague",
  "cod": 200
}|}

let expected =
  {|type clouds = {
  all : int;
} [@@deriving yojson]

type coord = {
  lon : float;
  lat : float;
} [@@deriving yojson]

type main = {
  temp_min : int;
  temp_max : int;
  temp : int;
  pressure : int;
  humidity : int;
} [@@deriving yojson]

type sys = {
  type_ : int[@key "type"];
  sunset : int;
  sunrise : int;
  message : float;
  id : int;
  country : string;
} [@@deriving yojson]

type weather_elt = {
  main : string;
  id : int;
  icon : string;
  description : string;
} [@@deriving yojson]

type wind = {
  speed : float;
  deg : int;
} [@@deriving yojson]

type t = {
  wind : wind;
  weather : weather_elt list;
  sys : sys;
  name : string;
  main : main;
  id : int;
  dt : int;
  coord : coord;
  cod : int;
  clouds : clouds;
  base : string;
} [@@deriving yojson]
|}

let make_gen_types_test ~name ~input ~expected =
  let actual = gen_types input in
  name >:: fun _ -> assert_equal expected actual ~printer:(fun x -> x)

let tests =
  "gen_types test suite"
  >::: [ make_gen_types_test ~name:"web test" ~input:json ~expected ]

let _ = run_test_tt_main tests
