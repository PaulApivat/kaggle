import pandas
flights = pandas.read_csv("flights.csv")
flights = flights[flights['dest'] == 'ORD']
flights = flights[['carrier', 'dep_delay', 'arr_delay']]
flights = flights.dropna()
print(flights.head())