import pyodbc
# from dotenv import load_dotenv
from azure.identity import AzureCliCredential

from sqlalchemy.engine import URL
from sqlalchemy import create_engine

import struct
import os

# load_dotenv()

# get port from environment variable
PORT = os.getenv("PORT")
PWD = os.getenv("PWD")
HOST = os.getenv("HOST")
USER = os.getenv("USER")
DB = os.getenv("DB")

def db_connect():

    # construct connection string
    connection_string = f"postgresql+psycopg2://{USER}:{PWD}@{HOST}:{PORT}/{DB}"
    # print(connection_string)
    try:
        engine = create_engine(connection_string, echo=True)
        conn = engine.connect()
    except pyodbc.InterfaceError as ex:
            raise ex
        
    return conn

def write_to_db(df, table_name, schema = 'public', append=False):
    conn = db_connect()
    if append:
        df.to_sql(table_name, conn, schema = schema, if_exists='append', index=False)
    else:
        df.to_sql(table_name, conn, schema = schema, if_exists='replace', index=False)
    conn.close()