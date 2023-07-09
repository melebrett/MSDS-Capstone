package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"os"

	"cloud.google.com/go/cloudsqlconn"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/stdlib"
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

const url = "https://feeds.datagolf.com/get-player-list?file_format=json&key="

var players []Player

func GetAPIrequest(url string) []Player {

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

	fmt.Println("Response Body:", string(body))

	if err := json.Unmarshal(body, &players); err != nil {
		fmt.Println("Error decoding JSON: ", err)
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

// connect to posgres database
func DbConnect() (*sql.DB, error) {
	mustGetenv := func(k string) string {
		v := os.Getenv(k)
		if v == "" {
			log.Fatalf("Fatal Error in connect_connector.go: %s environment variable not set.\n", k)
		}
		return v
	}

	var (
		dbUser                 = mustGetenv("USER")     // e.g. 'my-db-user'
		dbPwd                  = mustGetenv("PWD")      // e.g. 'my-db-password'
		dbName                 = mustGetenv("DB")       // e.g. 'my-database'
		instanceConnectionName = mustGetenv("INSTANCE") // e.g. 'project:region:instance'
	)

	dsn := fmt.Sprintf("user=%s password=%s database=%s", dbUser, dbPwd, dbName)
	config, err := pgx.ParseConfig(dsn)
	if err != nil {
		return nil, err
	}
	var opts []cloudsqlconn.Option
	d, err := cloudsqlconn.NewDialer(context.Background(), opts...)
	if err != nil {
		return nil, err
	}
	// Use the Cloud SQL connector to handle connecting to the instance.
	// This approach does *NOT* require the Cloud SQL proxy.
	config.DialFunc = func(ctx context.Context, network, instance string) (net.Conn, error) {
		return d.Dial(ctx, instanceConnectionName)
	}
	dbURI := stdlib.RegisterConnConfig(config)
	dbPool, err := sql.Open("pgx", dbURI)
	if err != nil {
		return nil, fmt.Errorf("sql.Open: %v", err)
	}
	return dbPool, nil
}

func RefreshTable() {
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

func WriteTable(Players []Player) {
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

	err := godotenv.Load()
	if err != nil {
		log.Fatal("Error loading .env file")
	}

	API_KEY := os.Getenv("API_KEY")

	// paste the url and api key
	full_url := url + API_KEY

	fmt.Println("calling API")
	GetAPIrequest(full_url)

	// SavePlayersJSON("players.json")

	fmt.Println("refreshing table")
	RefreshTable()

	fmt.Println("writing table")
	WriteTable(players)

	// fmt.Println(players)

}
