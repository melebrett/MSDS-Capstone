import pandas as pd
import numpy as np
# import matplotlib.pyplot as plt
# import seaborn as sns

import warnings
import utils

def clean_rounds(overwrite = False):
    
    conn = utils.db_connect()

    df_rounds = pd.read_sql("select * from public.rounds", conn)
    df_events = pd.read_sql("select * from public.events", conn)

    # convert event_id to int
    df_rounds['event_id'] = df_rounds['event_id'].astype(int)
    # join rounds to events
    df = pd.merge(df_rounds,
                df_events[['event_id', 'calendar_year', 'tour', 'has_sg', 'has_traditional_stats']],
                    left_on= ['event_id','year', 'tour'], right_on=['event_id','calendar_year', 'tour'], how='left')

    # clean some stuff up
    # if has_traditional_stats is 'no', then make the columns null
    df.loc[df['has_traditional_stats'] == 'no', ['driving_acc', 'driving_dist', 'gir', 'scrambling', 'prox_rgh', 'prox_fw', 'great_shots', 'poor_shots']] = np.nan
    df.loc[df['has_sg'] == 'no', ['sg_putt', 'sg_arg', 'sg_app', 'sg_off_tee', 'sg_t2g', 'sg_total']] = np.nan
    # convert to bool
    df['has_traditional_stats'] = df['has_traditional_stats'] == 'yes'
    df['has_sg'] = df['has_sg'] == 'yes'
    # convert teetime in format '12:54pm' to time, if not missing
    df['teetime'] =  pd.to_datetime(df['teetime'], format='%I:%M%p', errors='coerce').dt.time
    df.drop(['calendar_year'], axis=1, inplace=True)

    drop_statement = """
    DROP TABLE IF EXISTS gold.rounds;
    """

    create_statement = """
    CREATE TABLE gold.rounds (
        tour varchar(10),
        year int,
        season int,
        event_id int,
        event_name varchar(100),
        course_name varchar(100),
        course_num int,
        course_par int,
        dg_id int,
        player_name varchar(100),
        fin_text varchar(10),
        round_num int,
        start_hole int,
        teetime time,
        round_score int,
        sg_putt float,
        sg_arg float,
        sg_app float,
        sg_off_tee float,
        sg_t2g float,
        sg_total float,
        driving_acc float,
        driving_dist float,
        gir float,
        scrambling float,
        prox_rgh float,
        prox_fw float,
        great_shots int,
        poor_shots int,
        has_traditional_stats boolean,
        has_sg boolean
    );
    """

    if overwrite:
        conn.execute(drop_statement)
        conn.execute(create_statement)
        
    else:
        existing_rounds = pd.read_sql("select * from gold.rounds", conn)
        # filter out existing rounds based on event_id, year, tour, dg_id and round
        df = df[~df[['event_id', 'year', 'tour', 'dg_id', 'round_num']].isin(existing_rounds[['event_id', 'year', 'tour', 'dg_id', 'round_num']]).all(1)]
        df.drop_duplicates(inplace=True)

    if len(df) > 0:
        utils.write_to_db(df, 'rounds', 'gold', append=True)
    else:
        warnings.warn("no new rounds detected")
    
    conn.close()


def clean_events(overwrite = False):

    conn = utils.db_connect()

    df = pd.read_sql("select * from events", conn)

    df['has_traditional_stats'] = df['has_traditional_stats'] == 'yes'
    df['has_sg'] = df['has_sg'] == 'yes'
    df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%d')

    drop_statement = """
    DROP TABLE IF EXISTS gold.events;
    """

    create_statement = """
    CREATE TABLE gold.events (
        event_id int,
        event_name varchar(100),
        calendar_year int,
        date date,
        tour varchar(10),
        has_sg boolean,
        has_traditional_stats boolean
    );
    """

    if overwrite:
        conn.execute(drop_statement)
        conn.execute(create_statement)
    else:
        existing_events = pd.read_sql("select * from gold.events", conn)
        # filter out existing events based on event_id
        df = df[~df[['event_id', 'calendar_year']].isin(existing_events[['event_id', 'calendar_year']]).all(1)]
        df.drop_duplicates(inplace=True)

    if len(df > 0):
        utils.write_to_db(df, 'events', 'gold', append=True)
    else:
        warnings.warn("no new events detected")

    conn.close()

def clean_players(overwrite = False):

    conn = utils.db_connect()

    df = pd.read_sql("select * from public.players", conn)

    df['dg_id'] = df['dg_id'].astype(int)
    df['amateur'] = df['amateur'] == 'yes'

    drop_statement = """
    DROP TABLE IF EXISTS gold.players;
    """

    create_statement = """
    CREATE TABLE gold.players (
        dg_id int,
        amateur boolean,
        name varchar(100),
        country varchar(100),
        country_code varchar(10)
    );
    """

    if overwrite:
        conn.execute(drop_statement)
        conn.execute(create_statement)
    else:
        existing_players = pd.read_sql("select * from gold.players", conn)
        df = df[~df[['dg_id']].isin(existing_players[['dg_id']]).all(1)]
        df.drop_duplicates(inplace=True)

    # write the dataframe to the database
    if len(df) > 0:
        utils.write_to_db(df, 'players', schema='gold', append=True)

    conn.close()

def clean_rankings(overwrite = False):
    conn = utils.db_connect()

    df = pd.read_sql("select * from public.rankings", conn)

    # dg_id to int
    df['dg_id'] = df['dg_id'].astype(int)
    # amateur to bool
    df['amateur'] = df['amateur'] == 'yes'
    # updated_at to datetime
    df['updated_at'] = pd.to_datetime(df['updated_at'], format='%Y-%m-%d %H:%M:%S')

    drop_statement = """
    DROP TABLE IF EXISTS gold.rankings;
    """

    create_statement = """
    CREATE TABLE gold.rankings (
        dg_id int,
        amateur boolean,
        player varchar(100),
        primary_tour varchar(10),
        country varchar(100),
        dg_rank int,
        dg_skill_estimate numeric,
        owgr_rank int,
        updated_at timestamp
    );
    """

    if overwrite:
        conn.execute(drop_statement)
        conn.execute(create_statement)
    else:
        existing_rankings = pd.read_sql("select * from gold.rankings", conn)
        df = df[~df[['updated_at']].isin(existing_rankings[['updated_at']]).all(1)]
        df.drop_duplicates(inplace=True)

    # write the dataframe to the database
    if len(df) > 0:
        utils.write_to_db(df, 'rankings', schema='gold', append=True)
    else:
        warnings.warn("no new rankings detected")
    
    conn.close()

def clean_earnings(overwrite = False):

    conn = utils.db_connect()

    df = pd.read_sql("select * from public.earnings", conn)

    # drop "field average"
    df = df[df['Player'].str.lower() != 'field average']
    # take only season after the hyphen, if it contains a hyphen
    df['Season'] = df['Season'].apply(lambda x: x.split('-')[1] if '-' in x else x)
    # convert to int
    df['Season'] = df['Season'].astype(int)
    # rank to int
    df['Rank'] = df['Rank'].astype(int)
    # rename columns
    df.rename(columns={'Player': 'name', 'Season': 'season', 'Rank': 'rank', 'Money': 'money', 'Tournament': 'event_name'}, inplace=True)

    drop_statement = """
    DROP TABLE IF EXISTS gold.winnings;
    """

    create_statement = """
    CREATE TABLE gold.winnings (
        season int,
        event_name varchar(100),
        rank int,
        name varchar(100),
        money int
    );
    """

    if overwrite:
        conn.execute(drop_statement)
        conn.execute(create_statement)
    else:
        existing_earnings = pd.read_sql("select * from gold.winnings", conn)
        df = df[~df[['season', 'event_name', 'rank']].isin(existing_earnings[['season', 'event_name', 'rank']]).all(1)]
        df.drop_duplicates(inplace=True)

    # write the dataframe to the database
    if len(df) > 0:
        utils.write_to_db(df, 'winnings', schema='gold', append=True)
    else:
        warnings.warn("no new earnings detected")
    
    conn.close()

def clean_bio(overwrite = False):

    conn = utils.db_connect()

    df = pd.read_sql("select * from public.espn_bio", conn)

    # birthdate to date
    df['birthdate'] = pd.to_datetime(df['birthdate'], format='%m/%d/%Y')
    # turned_pro to int, if not missing or "None"
    df['turned_pro'] = df['turned_pro'].apply(lambda x: int(x) if x is not None else pd.NA)
    df['turned_pro'] = df['turned_pro'].astype('Int64')
    # espn_id to int
    df['espn_id'] = df['espn_id'].astype('Int64')
    # rename href
    df.rename(columns={'href': 'link'}, inplace=True)

    drop_statement = """
    DROP TABLE IF EXISTS gold.player_bio;
    """

    create_statement = """
    CREATE TABLE gold.player_bio (
        espn_id int,
        birthdate date,
        birthplace varchar(100),
        college varchar(100),
        swing varchar(100),
        turned_pro int,
        link varchar(150)
    );
    """

    if overwrite:
        conn.execute(drop_statement)
        conn.execute(create_statement)
    else:
        existing_bio = pd.read_sql("select * from gold.player_bio", conn)
        df = df[~df[['espn_id']].isin(existing_bio[['espn_id']]).all(1)]
        df.drop_duplicates(inplace=True)

    # write the dataframe to the database
    if len(df) > 0:
        utils.write_to_db(df, 'player_bio', schema='gold', append=True)
    else:
        warnings.warn("no new bios detected")

    conn.close()


def clean_stats(overwrite = False):

    conn = utils.db_connect()

    df = pd.read_sql("select * from public.espn_stats", conn)

    # strip non numeric characters from age column
    df['age'] = df['age'].str.replace(r'\D', '')
    df['age'] = df['age'].apply(lambda x: x if x != '' else None).astype('float64')

    # convert to int
    df['espn_id'] = df['espn_id'].astype('Int64')
    df['season'] = df['season'].astype('Int64')
    df['rk'] = df['rk'].astype('Int64')
    df['age'] = df['age'].astype('Int64')
    df['earnings'] = df['earnings'].astype('Int64')
    df['cup'] = df['cup'].astype('Int64')
    df['evnts'] = df['evnts'].astype('Int64')
    df['rnds'] = df['rnds'].astype('Int64')
    df['cuts'] = df['cuts'].astype('Int64')
    df['top10'] = df['top10'].astype('Int64')
    df['wins'] = df['wins'].astype('Int64')

    # convert to float
    df['score'] = df['score'].astype('float')
    df['ddis'] = df['ddis'].astype('float')
    df['dacc'] = df['dacc'].astype('float')
    df['gir'] = df['gir'].astype('float')
    df['putts'] = df['putts'].astype('float')
    df['sand'] = df['sand'].astype('float')
    df['birds'] = df['birds'].astype('float')

    # gir, dacc, sand to %
    df['gir'] = df['gir'] / 100
    df['dacc'] = df['dacc'] / 100
    df['sand'] = df['sand'] / 100

    # rename columns
    df.rename(columns={
        'season': 'season',
        'rk': 'rank', 
        'age': 'season_age',
        'earnings': 'total_earnings',
        'evnts': 'events',
        'rnds': 'rounds',
        'cuts': 'cuts_made',
        'top10': 'top_10',
        'wins': 'wins',
        'score': 'avg_score',
        'ddis': 'avg_ddis',
        'dacc': 'dacc_pct',
        'gir': 'gir_pct',
        'putts': 'avg_putts_per_hole',
        'sand': 'sand_save_pct',
        'birds': 'avg_birdies_per_round',
        'cup': 'fedex_cup_points'}, inplace=True)

    drop_statement = """
    DROP TABLE IF EXISTS gold.player_stats;
    """

    create_statement = """
    CREATE TABLE gold.player_stats (
        espn_id int,
        name varchar(100),
        season int,
        rank int,
        season_age int,
        total_earnings int,
        fedex_cup_points int,
        events int,
        rounds int,
        cuts_made int,
        top_10 int,
        wins int,
        avg_score float,
        avg_ddis float,
        dacc_pct float,
        gir_pct float,
        avg_putts_per_hole float,
        sand_save_pct float,
        avg_birdies_per_round float
    );
    """

    if overwrite:
        conn.execute(drop_statement)
        conn.execute(create_statement)

    else:
        existing_stats = pd.read_sql("select * from gold.player_stats", conn)
        df = df[~df[['espn_id', 'season']].isin(existing_stats[['espn_id', 'season']]).all(1)]
        df.drop_duplicates(inplace=True)
    
    # write the dataframe to the database
    if len(df) > 0:
        utils.write_to_db(df, 'player_stats', schema='gold', append=True)
    else:
        warnings.warn("no new stats detected")
    
    conn.close()


def main():
    try:
        clean_rounds(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update rounds: {ex}")

    try:
        clean_events(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update events: {ex}")

    try:
        clean_players(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update players: {ex}")

    try:
        clean_rankings(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update rankings: {ex}")

    try:
        clean_earnings(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update earnings: {ex}")

    try:
        clean_bio(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update bio: {ex}")

    try:
        clean_stats(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update stats: {ex}")
    
    print("done.")

if __name__ == "__main__":
    main()
