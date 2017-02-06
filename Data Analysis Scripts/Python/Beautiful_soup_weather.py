import urllib2
import csv
import pytz
import datetime

# Create a file called weatherORD_2013_now.csv
f = open('weatherORD_2013_now.csv', 'wb')
#configure the csv writer
CSVwriter = csv.writer(f, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)

#boolean to check if this is the first day being entered in to csv file. If it is, headers are written in. If not, they are left out.
isFirst = True


# Iterate through year, month, and day
for y in range(2013, 2017):
    for m in range(1, 13):
        for d in range(1, 32):
            # Check if leap year
            if y%400 == 0:
                leap = True
            elif y%100 == 0:
                leap = False
            elif y%4 == 0:
                leap = True
            else:
                leap = False

            # Check if already gone through month
            if (m == 2 and leap and d > 29):
                continue
            elif (m == 2 and d > 28):
                continue
            elif (m in [4, 6, 9, 10] and d > 30):
                continue

            #this is the URL that will be manipulaed to get the CSV data
            url = "https://www.wunderground.com/history/airport/KORD/" +str(y)+ "/" + str(m) + "/" + str(d) + "/DailyHistory.html?HideSpecis=1&format=1"
            response = urllib2.urlopen(url)
            cr = csv.reader(response)

            #read in first line. This resource always sends back an empty line in the first row
            first_blank_line = next(cr)
            #these are the headings of the table. Only included once
            csv_headings = next(cr)

            #if this is the first day being read in, include headings
            if isFirst == True:
                csv_headings.append('ChicagoTime') #append a new column (list item)
                csv_headings[-2] = csv_headings[-2].replace("<br />","") #make sure there is no "<br />" tag in the last heading (prior to append)
                CSVwriter.writerow(csv_headings) #write headings to file
                isFirst = False #make sure headings are only written to file once

            #iterate through the rest of the table
            for row in cr:
                #replace "<br />" tag found in last list element or every row
                row[-1] = row[-1].replace("<br />","")
                #convert string to datetime object
                naive_date = datetime.datetime.strptime(row[-1], "%Y-%m-%d %H:%M:%S")
                #localize the returned time to UTC. This makes the datetime timezone-aware
                date_aware_utc = pytz.utc.localize(naive_date)
                #define the timezone that the UTC datetime needs to be adjusted to
                CHItz = pytz.timezone('America/Chicago')
                #convert to Chicago date, leaving off the timezone info. This adjusts the time for the timezone before making it naive
                chi_date = date_aware_utc.astimezone(CHItz).replace(tzinfo=None)
                #append the adjusted naive date to the row (for ChicagoTime)
                row.append(chi_date)
                #write row
                CSVwriter.writerow(row)

#Close file.
f.close()
