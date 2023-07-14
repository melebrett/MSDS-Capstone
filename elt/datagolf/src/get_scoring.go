package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/joho/godotenv"
	_ "github.com/lib/pq"
)

type RoundScore struct {
	CourseName  string  `json:"course_name"`
	CourseNum   int     `json:"course_num"`
	CoursePar   int     `json:"course_par"`
	DrivingAcc  float64 `json:"driving_acc"`
	DrivingDist float64 `json:"driving_dist"`
	GIR         float64 `json:"gir"`
	GreatShots  float64 `json:"great_shots"`
	PoorShots   float64 `json:"poor_shots"`
	ProxFW      float64 `json:"prox_fw"`
	ProxRGH     float64 `json:"prox_rgh"`
	Score       int     `json:"score"`
	Scrambling  float64 `json:"scrambling"`
	SGApp       float64 `json:"sg_app"`
	SGArg       float64 `json:"sg_arg"`
	SGOTT       float64 `json:"sg_ott"`
	SGPutt      float64 `json:"sg_putt"`
	SGT2G       float64 `json:"sg_t2g"`
	SGTotal     float64 `json:"sg_total"`
	StartHole   int     `json:"start_hole"`
	TeeTime     string  `json:"teetime"`
}

type ScoreInfo struct {
	DGID       int        `json:"dg_id"`
	FinText    string     `json:"fin_text"`
	PlayerName string     `json:"player_name"`
	Round1     RoundScore `json:"round_1"`
	Round2     RoundScore `json:"round_2"`
	Round3     RoundScore `json:"round_3"`
	Round4     RoundScore `json:"round_4"`
}

type ScoreResponse struct {
	EventCompleted      string      `json:"event_completed"`
	EventID             string      `json:"event_id"`
	EventName           string      `json:"event_name"`
	Scores              []ScoreInfo `json:"scores"`
	Season              int         `json:"season"`
	HasSg               string      `json:"sg_categories"`
	Tour                string      `json:"tour"`
	HasTraditionalStats string      `json:"traditional_stats"`
	CalendarYear        int         `json:"year"`
}

var AllScores []ScoreResponse

type PlayerRoundScore struct {
	Tour        string
	Year        int
	Season      int
	EventName   string
	EventID     string
	PlayerName  string
	DGID        int
	FinText     string
	RoundNum    int
	CourseName  string
	CourseNum   int
	CoursePar   int
	StartHole   int
	TeeTime     string
	Score       int
	SGPutt      float64
	SGArg       float64
	SGApp       float64
	SGOtt       float64
	SGTtg       float64
	SGTotal     float64
	DrivingDist float64
	DrivingAcc  float64
	Gir         float64
	Scrambling  float64
	ProxRGH     float64
	ProxFW      float64
	GreatShots  int
	PoorShots   int
}

// var PlayerRoundScores []PlayerRoundScore

// loop through the event struct and call the API for each event
func GetRequestScores(url string, key string, events []Event) []ScoreResponse {

	var event_id string
	var tour string
	var calendar_year string
	var full_url string
	var this_event ScoreResponse

	// counter := 0
	for _, v := range events {

		// only get PGA and KFT events
		if v.Tour == "pga" || v.Tour == "kft" {

			// convert the event value to a string
			event_id = strconv.Itoa(v.EventId)
			tour = v.Tour
			calendar_year = strconv.Itoa(v.CalendarYear)

			full_url = url + tour + "&event_id=" + event_id + "&year=" + calendar_year + "&file_format=json&key=" + key

			resp, err := http.Get(full_url)
			if err != nil {
				log.Fatal(err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusOK {
				log.Fatal("Received non-OK status code:", resp.StatusCode)
			}

			body, err := io.ReadAll(resp.Body)

			if err != nil {
				log.Fatal(err)
			}

			if err := json.Unmarshal(body, &this_event); err != nil {
				fmt.Println("Error decoding JSON: ", err)
				fmt.Println("Response Body:", string(body))
				log.Fatal(err)
			}

			AllScores = append(AllScores, this_event)

			// break the loop for testing
			// counter++
			// if counter > 1 {
			// 	break
			// }

		}
	}

	return AllScores

}

// function to flatten the Score responses into a slice of PlayerRoundScore structs
func FlattenScores(scores []ScoreResponse) []PlayerRoundScore {

	var season int
	var year int
	var tour string
	var event_id string
	var dg_id int
	var fin_text string
	var player_name string
	var PlayerRoundScores []PlayerRoundScore

	for _, v := range scores {

		season = v.Season
		year = v.CalendarYear
		tour = v.Tour
		event_id = v.EventID

		for _, s := range v.Scores {

			// extract player info
			dg_id = s.DGID
			fin_text = s.FinText
			player_name = s.PlayerName

			// for round 1, if RoundScore is not empty append his scores to the PlayerRoundScore slice
			if s.Round1 != (RoundScore{}) {

				PlayerRoundScores = append(PlayerRoundScores, PlayerRoundScore{
					Tour:        tour,
					Year:        year,
					Season:      season,
					EventName:   v.EventName,
					EventID:     event_id,
					PlayerName:  player_name,
					DGID:        dg_id,
					FinText:     fin_text,
					RoundNum:    1,
					CourseName:  s.Round1.CourseName,
					CourseNum:   s.Round1.CourseNum,
					CoursePar:   s.Round1.CoursePar,
					StartHole:   s.Round1.StartHole,
					TeeTime:     s.Round1.TeeTime,
					Score:       s.Round1.Score,
					SGPutt:      s.Round1.SGPutt,
					SGArg:       s.Round1.SGArg,
					SGApp:       s.Round1.SGApp,
					SGOtt:       s.Round1.SGOTT,
					SGTtg:       s.Round1.SGT2G,
					SGTotal:     s.Round1.SGTotal,
					DrivingDist: s.Round1.DrivingDist,
					DrivingAcc:  s.Round1.DrivingAcc,
					Gir:         s.Round1.GIR,
					Scrambling:  s.Round1.Scrambling,
					ProxRGH:     s.Round1.ProxRGH,
					ProxFW:      s.Round1.ProxFW,
					GreatShots:  int(s.Round1.GreatShots),
					PoorShots:   int(s.Round1.PoorShots),
				})

			}
			// same thing for round 2
			if s.Round2 != (RoundScore{}) {

				PlayerRoundScores = append(PlayerRoundScores, PlayerRoundScore{
					Tour:        tour,
					Year:        year,
					Season:      season,
					EventName:   v.EventName,
					EventID:     event_id,
					PlayerName:  player_name,
					DGID:        dg_id,
					FinText:     fin_text,
					RoundNum:    2,
					CourseName:  s.Round2.CourseName,
					CourseNum:   s.Round2.CourseNum,
					CoursePar:   s.Round2.CoursePar,
					StartHole:   s.Round2.StartHole,
					TeeTime:     s.Round2.TeeTime,
					Score:       s.Round2.Score,
					SGPutt:      s.Round2.SGPutt,
					SGArg:       s.Round2.SGArg,
					SGApp:       s.Round2.SGApp,
					SGOtt:       s.Round2.SGOTT,
					SGTtg:       s.Round2.SGT2G,
					SGTotal:     s.Round2.SGTotal,
					DrivingDist: s.Round2.DrivingDist,
					DrivingAcc:  s.Round2.DrivingAcc,
					Gir:         s.Round2.GIR,
					Scrambling:  s.Round2.Scrambling,
					ProxRGH:     s.Round2.ProxRGH,
					ProxFW:      s.Round2.ProxFW,
					GreatShots:  int(s.Round2.GreatShots),
					PoorShots:   int(s.Round2.PoorShots),
				})

			}
			if s.Round3 != (RoundScore{}) {

				PlayerRoundScores = append(PlayerRoundScores, PlayerRoundScore{
					Tour:        tour,
					Year:        year,
					Season:      season,
					EventName:   v.EventName,
					EventID:     event_id,
					PlayerName:  player_name,
					DGID:        dg_id,
					FinText:     fin_text,
					RoundNum:    3,
					CourseName:  s.Round3.CourseName,
					CourseNum:   s.Round3.CourseNum,
					CoursePar:   s.Round3.CoursePar,
					StartHole:   s.Round3.StartHole,
					TeeTime:     s.Round3.TeeTime,
					Score:       s.Round3.Score,
					SGPutt:      s.Round3.SGPutt,
					SGArg:       s.Round3.SGArg,
					SGApp:       s.Round3.SGApp,
					SGOtt:       s.Round3.SGOTT,
					SGTtg:       s.Round3.SGT2G,
					SGTotal:     s.Round3.SGTotal,
					DrivingDist: s.Round3.DrivingDist,
					DrivingAcc:  s.Round3.DrivingAcc,
					Gir:         s.Round3.GIR,
					Scrambling:  s.Round3.Scrambling,
					ProxRGH:     s.Round3.ProxRGH,
					ProxFW:      s.Round3.ProxFW,
					GreatShots:  int(s.Round3.GreatShots),
					PoorShots:   int(s.Round3.PoorShots),
				})

			}
			if s.Round4 != (RoundScore{}) {

				PlayerRoundScores = append(PlayerRoundScores, PlayerRoundScore{
					Tour:        tour,
					Year:        year,
					Season:      season,
					EventName:   v.EventName,
					EventID:     event_id,
					PlayerName:  player_name,
					DGID:        dg_id,
					FinText:     fin_text,
					RoundNum:    4,
					CourseName:  s.Round4.CourseName,
					CourseNum:   s.Round4.CourseNum,
					CoursePar:   s.Round4.CoursePar,
					StartHole:   s.Round4.StartHole,
					TeeTime:     s.Round4.TeeTime,
					Score:       s.Round4.Score,
					SGPutt:      s.Round4.SGPutt,
					SGArg:       s.Round4.SGArg,
					SGApp:       s.Round4.SGApp,
					SGOtt:       s.Round4.SGOTT,
					SGTtg:       s.Round4.SGT2G,
					SGTotal:     s.Round4.SGTotal,
					DrivingDist: s.Round4.DrivingDist,
					DrivingAcc:  s.Round4.DrivingAcc,
					Gir:         s.Round4.GIR,
					Scrambling:  s.Round4.Scrambling,
					ProxRGH:     s.Round4.ProxRGH,
					ProxFW:      s.Round4.ProxFW,
					GreatShots:  int(s.Round4.GreatShots),
					PoorShots:   int(s.Round4.PoorShots),
				})

			}

		}
	}

	return PlayerRoundScores

}

// function for saving JSON file for testing - eliminate excessive API calls
func SaveScoresJSON(filename string) {
	content, err := json.Marshal(AllScores)
	if err != nil {
		log.Fatalf("Error while marshaling struct: %v", err)
	}
	err = os.WriteFile(filename, content, 0777)
	if err != nil {
		log.Fatalf("Error while writing to json file: %v", err)
	}
}

// function for saving JSON file for testing - eliminate excessive API calls
func SavePRSJSON(filename string, prs []PlayerRoundScore) {
	content, err := json.Marshal(prs)
	if err != nil {
		log.Fatalf("Error while marshaling struct: %v", err)
	}
	err = os.WriteFile(filename, content, 0777)
	if err != nil {
		log.Fatalf("Error while writing to json file: %v", err)
	}
}

func RefreshScoresTable() {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	dropTableStatement := "DROP TABLE IF EXISTS rounds;"

	_, err = db.Exec(dropTableStatement)
	if err != nil {
		panic(err)
	}

	createTableStatement := `CREATE TABLE rounds (
					tour TEXT,
					year INT,
					season INT,
					event_name TEXT,
					event_id TEXT,
					player_name TEXT,
					dg_id INT,
					fin_text TEXT,
					round_num INT,
					course_name TEXT,
					course_num INT,
					course_par INT,
					start_hole INT,
					teetime TEXT,
					round_score INT,
					sg_putt FLOAT,
					sg_arg FLOAT,
					sg_app FLOAT,
					sg_off_tee FLOAT,
					sg_t2g FLOAT,
					sg_total FLOAT,
					driving_dist FLOAT,
					driving_acc FLOAT,
					gir FLOAT,
					scrambling FLOAT,
					prox_rgh FLOAT,
					prox_fw FLOAT,
					great_shots FLOAT,
					poor_shots FLOAT
		);`

	_, err = db.Exec(createTableStatement)
	if err != nil {
		panic(err)
	}
}

func WriteScoresTable(PlayerRoundScores []PlayerRoundScore) {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	insertStatement := `INSERT INTO rounds (tour, year, season, event_name, event_id, player_name, dg_id, fin_text, round_num, course_name, course_num, course_par, start_hole, teetime, round_score, sg_putt, sg_arg, sg_app, sg_off_tee, sg_t2g, sg_total, driving_dist, driving_acc, gir, scrambling, prox_rgh, prox_fw, great_shots, poor_shots)
							values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29);`

	for _, v := range PlayerRoundScores {

		_, err = db.Exec(insertStatement, v.Tour, v.Year, v.Season, v.EventName, v.EventID, v.PlayerName, v.DGID, v.FinText, v.RoundNum, v.CourseName, v.CourseNum, v.CoursePar, v.StartHole, v.TeeTime, v.Score, v.SGPutt, v.SGArg, v.SGApp, v.SGOtt, v.SGTtg, v.SGTotal, v.DrivingDist, v.DrivingAcc, v.Gir, v.Scrambling, v.ProxRGH, v.ProxFW, v.GreatShots, v.PoorShots)
		if err != nil {
			fmt.Printf("Error inserting record: Season %v, Event %v, Player %v, Round %v\n", v.Season, v.EventName, v.PlayerName, v.RoundNum)
			fmt.Println(err)
			// print the values that were supposed to be inserted
			fmt.Println(v.Tour, v.Year, v.Season, v.EventName, v.EventID, v.PlayerName, v.DGID, v.FinText, v.RoundNum, v.CourseName, v.CourseNum, v.CoursePar, v.StartHole, v.TeeTime, v.Score, v.SGPutt, v.SGArg, v.SGApp, v.SGOtt, v.SGTtg, v.SGTotal, v.DrivingDist, v.DrivingAcc, v.Gir, v.Scrambling, v.ProxRGH, v.ProxFW, v.GreatShots, v.PoorShots)
			panic(err)
		}
	}
}

func LoadScores(events []Event) {

	err := godotenv.Load()
	if err != nil {
		log.Fatal("Error loading .env file")
	}

	API_KEY := os.Getenv("API_KEY")

	// paste the url and api key
	url := "https://feeds.datagolf.com/historical-raw-data/rounds?tour="

	fmt.Println("calling scores endpoint")
	GetRequestScores(url, API_KEY, events)
	// SaveScoresJSON("scores.json")

	// run flatten scores and get the resulting slice
	fmt.Println("flattening scores")
	PlayerRoundScores := FlattenScores(AllScores)

	// SavePRSJSON("prs.json", PlayerRoundScores)
	fmt.Println("refreshing rounds table")
	RefreshScoresTable()

	fmt.Println("writing rounds table")
	WriteScoresTable(PlayerRoundScores)

}
