
# Environment variables for connecting to
# one and only one MSSQL database.
# Thus, all the DB names are the same.
Sys.setenv(MYSQL_DB_LOG = "mydatabase")
Sys.setenv(MYSQL_DB_AUTOREPORT = "mydatabase")
Sys.setenv(MYSQL_DB_DATA = "mydatabase")
Sys.setenv(MYSQL_HOST = "mssql")
Sys.setenv(MYSQL_USER = "sa")
Sys.setenv(MYSQL_PASSWORD = "Your_password123")
Sys.setenv(DB_TYPE = "mssql")
Sys.setenv(MYSQL_DRIVER = "FreeTDS")
Sys.setenv(MYSQL_PORT = 1433)
