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

type Player struct {
	Amateur     int64  `json:"amateur"`
	Country     string `json:"country"`
	CountryCode string `json:"country_code"`
	DgId        int64  `json:"dg_id"`
	PlayerName  string `json:"player_name"`
}

var players []Player

func GetRequestPlayers(url string) []Player {

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

	if err := json.Unmarshal(body, &players); err != nil {
		fmt.Println("Error decoding JSON: ", err)
		fmt.Println("Response Body:", string(body))
	}

	return players
}

// function for saving JSON file for testing - eliminate excessive API calls
func SavePlayersJSON(filename string) {
	content, err := json.Marshal(players)
	if err != nil {
		log.Fatalf("Error while marshaling struct: %v", err)
	}
	err = os.WriteFile(filename, content, 0777)
	if err != nil {
		log.Fatalf("Error while writing to json file: %v", err)
	}
}

func RefreshPlayerTable() {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	dropTableStatement := "DROP TABLE IF EXISTS players;"

	_, err = db.Exec(dropTableStatement)
	if err != nil {
		panic(err)
	}

	createTableStatement := `CREATE TABLE players (
								dg_id                  INT PRIMARY KEY,
								amateur                  INT,
								country_code			TEXT,
								country   TEXT,
								name      TEXT
							);`

	_, err = db.Exec(createTableStatement)
	if err != nil {
		panic(err)
	}
}

func WritePlayerTable(Players []Player) {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	insertStatement := `INSERT INTO players (dg_id, amateur, country_code, country, name)
							values ($1, $2, $3, $4, $5)
							ON CONFLICT (dg_id) 
							DO NOTHING;`

	for _, v := range Players {
		_, err = db.Exec(insertStatement, v.DgId, v.Amateur, v.CountryCode, v.Country, v.PlayerName)
		if err != nil {
			fmt.Printf("Error inserting record, DgId = %v", v.DgId)
		}
	}
}

func LoadPlayers() {

	var url = "https://feeds.datagolf.com/get-player-list?file_format=json&key="

	err := godotenv.Load()
	if err != nil {
		log.Fatal("Error loading .env file")
	}

	API_KEY := os.Getenv("API_KEY")

	// paste the url and api key
	full_url := url + API_KEY

	fmt.Println("calling players endpoint")
	GetRequestPlayers(full_url)

	// SavePlayersJSON("players.json")

	fmt.Println("refreshing player table")
	RefreshPlayerTable()

	fmt.Println("writing player table")
	WritePlayerTable(players)

}
