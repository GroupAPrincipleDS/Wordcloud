library(DBI)

# Connect to PostgreSQL
con <- dbConnect(RPostgres::Postgres(), dbname='pds-project',
                 host = 'pds-group-project.carynrwc1v04.ap-southeast-1.rds.amazonaws.com',
                 port = 5432,
                 user = 'pdsgroupproject',
                 password = 'pdsadmin123')

# dataset that will be used in wordcloud	
t <- dbReadTable(con,'data_with_details')