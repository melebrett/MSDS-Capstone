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

    utils.write_to_db(df, 'rounds', 'gold', append=True)


def main():
    try:
        clean_rounds(overwrite=False)
    except Exception as ex:
        warnings.warn(f"failed to update rounds: {ex}")

if __name__ == "__main__":
    main()



