package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"

	"github.com/joho/godotenv"
	_ "github.com/lib/pq"
)

type PlayerRanking struct {
	Amateur         int     `json:"am"`
	Country         string  `json:"country"`
	DgRank          int     `json:"datagolf_rank"`
	DgId            int     `json:"dg_id"`
	DgSkillEstimate float64 `json:"dg_skill_estimate"`
	OwgrRank        int     `json:"owgr_rank"`
	Player          string  `json:"player_name"`
	PrimaryTour     string  `json:"primary_tour"`
}

type Rankings struct {
	LastUpdated string          `json:"last_updated"`
	Notes       string          `json:"notes"`
	Rankings    []PlayerRanking `json:"rankings"`
}

func GetRequestRankings(url string) []PlayerRanking {

	var rankings Rankings

	resp, err := http.Get(url)
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

	if err := json.Unmarshal(body, &rankings); err != nil {
		fmt.Println("Error decoding JSON: ", err)
		fmt.Println("Response Body:", string(body))
	}

	// loop through the values in rankings.PlayerRanking and append to PlayerRanking slice
	var players []PlayerRanking
	for _, value := range rankings.Rankings {
		players = append(players, value)
	}

	return players
}

func RefreshRankingsTable() {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	dropTableStatement := "DROP TABLE IF EXISTS rankings;"

	_, err = db.Exec(dropTableStatement)
	if err != nil {
		panic(err)
	}

	createTableStatement := `CREATE TABLE rankings (
		dg_id INT PRIMARY KEY,
		player VARCHAR(100),
		primary_tour VARCHAR(6),
		amateur INT,
		country VARCHAR(6),
		dg_rank INT,
		dg_skill_estimate FLOAT,
		owgr_rank INT,
		updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
	);`

	_, err = db.Exec(createTableStatement)
	if err != nil {
		panic(err)
	}
}

func WriteRankingsTable(Players []PlayerRanking) {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	insertStatement := `INSERT INTO rankings (dg_id, player, primary_tour, amateur, country, dg_rank, dg_skill_estimate, owgr_rank)
							values ($1, $2, $3, $4, $5, $6, $7, $8)
							ON CONFLICT (dg_id) 
							DO NOTHING;`

	for _, v := range Players {
		_, err = db.Exec(insertStatement, v.DgId, v.Player, v.PrimaryTour, v.Amateur, v.Country, v.DgRank, v.DgSkillEstimate, v.OwgrRank)
		if err != nil {
			fmt.Printf("Error inserting record, DgId = %v", v.DgId)
		}
	}
}

func LoadRankings(refresh bool) {

	var url = "https://feeds.datagolf.com/preds/get-dg-rankings?file_format=json&key="

	err := godotenv.Load()
	if err != nil {
		log.Fatal("Error loading .env file")
	}

	API_KEY := os.Getenv("API_KEY")

	// paste the url and api key
	full_url := url + API_KEY

	fmt.Println("calling rankings endpoint")
	player_rankings := GetRequestRankings(full_url)

	// SavePlayersJSON("players.json")

	if refresh {
		fmt.Println("refreshing rankings table")
		RefreshRankingsTable()
	}

	fmt.Println("writing rankings table")
	WriteRankingsTable(player_rankings)

}
