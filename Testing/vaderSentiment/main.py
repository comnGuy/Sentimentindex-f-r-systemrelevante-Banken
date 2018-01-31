import csv
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer


analyzer = SentimentIntensityAnalyzer()


testData = {}
with open('datenusanew.csv') as csvfile:
    readCSV = csv.reader(csvfile, delimiter=',')
    count = 0
    for row in readCSV:
        #print(row)
        #testData[count] = { 'X': row[0], 'full_text': row[1], 'sentimentOriginal': row[2]}
        testData[count] = { 'X': row[0], 'full_text': row[2], 'Weekday': row[15], 'Month': row[16], 'DayOfMonth': row[17], 'Hour': row[18], 'Min': row[19], 'Sec': row[20], 'Year': row[22]}
        #print({ 'X': row[0], 'full_text': row[2], 'Weekday': row[15], 'Month': row[16], 'DayOfMonth': row[17], 'Hour': row[18], 'Min': row[19], 'Sec': row[20], 'Year': row[22]})
        #print(row)
        count += 1

#print(testData)


f = open('daten_usa_new_sentiment_without0.csv', 'w')

#print(testData[3])

#f.write("X;full_text;Weekday,Month,DayOfMonth;Hour;Min;Sec;Year;sentiment\n")
for tweet in testData:
    #print(tweet)
    #print(testData[tweet])
    results = analyzer.polarity_scores(testData[tweet]['full_text'])
    #print()
    if results['compound'] != 0.0:
        f.write('"'+ str(testData[tweet]['Weekday']) + '";"' +
                str(testData[tweet]['Month']) + '";' +
                str(testData[tweet]['DayOfMonth']) + ';' +
                str(testData[tweet]['Hour']) + ';' +
                str(testData[tweet]['Min']) + ';' +
                str(testData[tweet]['Sec']) + ';' +
                str(testData[tweet]['Year']) + ';' +
                str(results['compound']) + '\n')
f.close()
