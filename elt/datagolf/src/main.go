package main

// import the players package

func main() {

	// run the LoadPlayers function from load_players.go
	LoadPlayers()

	// run the LoadEvents function from load_events.go
	LoadEvents()

	// run the LoadScores function from load_scores.go
	LoadScores(events)

}
