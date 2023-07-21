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

type Event struct {
	CalendarYear        int    `json:"calendar_year"`
	Date                string `json:"date"`
	EventId             int    `json:"event_id"`
	EventName           string `json:"event_name"`
	HasSg               string `json:"sg_categories"`
	Tour                string `json:"tour"`
	HasTraditionalStats string `json:"traditional_stats"`
}

var events []Event

func GetRequestEvents(url string) []Event {

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

	if err := json.Unmarshal(body, &events); err != nil {
		fmt.Println("Error decoding JSON: ", err)
		fmt.Println("Response Body:", string(body))
		log.Fatal(err)
	}

	return events
}

// function for saving JSON file for testing - eliminate excessive API calls
func SaveEventsJSON(filename string) {
	content, err := json.Marshal(events)
	if err != nil {
		log.Fatalf("Error while marshaling struct: %v", err)
	}
	err = os.WriteFile(filename, content, 0777)
	if err != nil {
		log.Fatalf("Error while writing to json file: %v", err)
	}
}

func RefreshEventTable() {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	dropTableStatement := "DROP TABLE IF EXISTS events;"

	_, err = db.Exec(dropTableStatement)
	if err != nil {
		panic(err)
	}

	createTableStatement := `CREATE TABLE events (
					calendar_year INT,
					date		  TEXT,
					event_id                  INT,
					event_name			TEXT,
					has_sg   TEXT,
					tour      TEXT,
					has_traditional_stats      TEXT
		);`

	_, err = db.Exec(createTableStatement)
	if err != nil {
		panic(err)
	}
}

func WriteEventTable(Events []Event) {
	db, err := DbConnect()
	if err != nil {
		log.Fatal(err)
	}

	defer db.Close()

	insertStatement := `INSERT INTO events (calendar_year, date, event_id, event_name, has_sg, tour, has_traditional_stats)
							values ($1, $2, $3, $4, $5, $6, $7);`

	for _, v := range Events {
		_, err = db.Exec(insertStatement, v.CalendarYear, v.Date, v.EventId, v.EventName, v.HasSg, v.Tour, v.HasTraditionalStats)
		if err != nil {
			fmt.Printf("Error inserting record: DgId %v Event %v date %v", v.EventId, v.EventName, v.Date)
		}
	}
}

func LoadEvents() {

	// set the url variable
	url := "https://feeds.datagolf.com/historical-raw-data/event-list?file_format=json&key="

	err := godotenv.Load()
	if err != nil {
		log.Fatal("Error loading .env file")
	}

	API_KEY := os.Getenv("API_KEY")

	// paste the url and api key
	full_url := url + API_KEY

	// make the api call
	fmt.Println("calling events endpoint")
	GetRequestEvents(full_url)

	// SaveEventsJSON("events.json")

	//write to database
	fmt.Println("refreshing events table")
	RefreshEventTable()

	fmt.Println("writing event table")
	WriteEventTable(events)

}
