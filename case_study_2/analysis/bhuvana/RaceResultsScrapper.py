import requests
from bs4 import BeautifulSoup
import csv
import time as t
import datetime as dt
import multiprocessing
import concurrent.futures
from joblib import Parallel, delayed
import sys
import traceback
import re

class RaceResultsScrapper:
    separator = base_url = divOption = pageOption = section = sex = utf = yearOption = header=""


    def __init__(self,url,urlOptionDictionary,header,years,separator):
        self.separator = separator
        self.base_url = url
        self.divOption = urlOptionDictionary["divOption"]
        self.pageOption = urlOptionDictionary["pageOption"]
        self.section = urlOptionDictionary["section"]
        self.sex = urlOptionDictionary["sex"]
        self.utf = urlOptionDictionary["utf"]
        self.yearOption = urlOptionDictionary["yearOption"]
        self.header = header

    def scrapeResultsforYears(self,y,maxPageLimit=1000):
        yearContent= list()
        #for y in yearRange:
        start = t.perf_counter()
        print("Processing year ",y)
        subPageContent = self.scrapePageContent(maxPageLimit,y)
        yearContent.append(subPageContent)
        end = t.perf_counter()
        print(f'Time Taken to process results for year {y} is {end-start} sec(s)')
        return yearContent



    def scrapePageContent(self,maxLimit,year):
        pageRows = list()
        try:
            for i in range(1,maxLimit):
                new_url = self.buildPageUrl(i,year)
                trContent = self.fetchWebContent(new_url)
                if (len(trContent) <= 0):
                    break
                else:
                    content = self.parseContent(trContent,year)
                    pageRows.append(content)
        except ValueError as error:
            print("Error occurred while processing url",new_url)
            print(traceback.format_exception(None, error, error.__traceback__),
                  file=sys.stderr, flush=True)
            raise error
        return pageRows



    def buildPageUrl(self,pageNum,yearNum):
            new_page = self.pageOption.replace("PAGENUM", str(pageNum))
            new_year = self.yearOption.replace("YEARNUM", str(yearNum))
            new_url = self.base_url + self.divOption + new_page + self.section + self.sex + self.utf + new_year
            return new_url

    def fetchWebContent(self,new_url):
        trContents = ""
        r = requests.get(new_url)
        soup = BeautifulSoup(r.text, "html.parser")
        content = soup.text
        if ("something went wrong (500)" in content):
            print(f'Page not found for url {new_url}')
            return ""
            #break;
        divContent = soup.find("div", {"class": "performances-list"})
        if type(divContent) != None.__class__:
            div = divContent.find("tr", {"class": "print-link-color"})
            if type(div) !=None.__class__:
                trContents = divContent.findAll("tr", {"class": "print-link-color"})
            else:
                print(f'Url {new_url} does not contain any result')
        #print(trContents)
        return trContents

    def parseContent(self,trContent,year):
        rows = list()
        try:
            for tr in trContent:
                name = tr.find("td", {"class": ""}).find("a").text
                name = name.replace('(W)','')
                name = re.sub('[^A-Za-z ]+', '', name) #remove special characters in name

                age = tr.find("td", {"class": "runner-page-vitals age"}).find("a").text
                time = tr.find("td", {"class": "runner-page-vitals side-by-side-vitals time"}).find("a").text
                pace = tr.find("td", {"class": "runner-page-vitals side-by-side-vitals pace"}).find("a").text
                pisTisTxt = tr.find("td", {"class": "runner-page-vitals side-by-side-vitals pis"}).find("a").text
                pisTis = pisTisTxt.split("/")
                pis=tis=""
                if(len(pisTis)==2):
                    pis = pisTis[0]
                    tis = pisTis[1]
                else :
                    pis = tis =pisTisTxt

                pidTidTxt = tr.find("td", {"class": "runner-page-vitals pid"}).find("a").text
                pidTid = pidTidTxt.split("/")
                pid=tid=""
                if (len(pidTid) == 2):
                    pid = pidTid[0]
                    tid = pidTid[1]
                else:
                    pid = tid = pidTidTxt

                division = tr.find("td", {"class": "runner-page-vitals division"}).find("a").text

                time = re.sub('[^0-9:]', '', time.strip())
                pace = re.sub('[^0-9:]', '', pace.strip())
                timeInSec = sum(int(x) * 60 ** i for i, x in enumerate(reversed(time.split(':'))))
                paceInSec = sum(int(x) * 60 ** i for i, x in enumerate(reversed(pace.split(':'))))

                hometownTxt = tr.find("td", {"class": "hometown"}).find("a").text
                state = country= hometown = ""
                ht = hometownTxt.split(",")
                if (len(ht) == 2):
                    hometown = ht[0]
                    state = ht[1]
                else :
                    country = ht[0]


                fields = [str(year), name, str(age), str(time), str(pace),str(timeInSec),str(paceInSec), str(pis),str(tis), str(division), str(pid),str(tid),
                          hometown,state,country]
                row = self.separator.join(fields)
                # print(row)
                rows.append(row)
        except ValueError as error:
            print(" Value Error occurred during parsing")
            print(str(error))
            print(traceback.format_exception(None, error, error.__traceback__), file=sys.stderr, flush=True)
            raise error
        return rows

def main():
    actualStart = t.perf_counter()
    num_cores = multiprocessing.cpu_count()
    print("Execution starting at ",dt.datetime.now())
    print("Num of cores : ",num_cores)
    url = 'http://www.cballtimeresults.org/performances?'
    urlOptionDictionary = {"divOption": "division=Overall+Women",
                                 "pageOption": "&page=PAGENUM",
                                 "section": "&section=10M",
                                 "sex": "&sex=W",
                                 "utf": "&utf8=%E2%9C%93",
                                 "yearOption": "&year=YEARNUM"}
    years = range(1999, 2013)
    separator=","
    headerFields = ["Year", "Name", "Age", "Time(HH:MM:SS)", "Pace(mm:ss)", "TotalTimeInSec","PaceInSec","Pis","Tis", "Division", "Pid","Tid", "Hometown","State","Country"]
    header = separator.join(headerFields)

    scrapper = RaceResultsScrapper(url, urlOptionDictionary, header,years,separator)
    #processed_result = Parallel(n_jobs=num_cores)(delayed(scrapper.scrapeResultsforYears)(year, 1000) for year in years)
    with concurrent.futures.ProcessPoolExecutor() as executor:
        processed_result = executor.map(scrapper.scrapeResultsforYears,years)

    with open('RaceResults99-2k12.csv', 'w', newline='') as file:
        writer = csv.writer(file,delimiter=';')
        writer.writerow([header,])
        for yearResults in processed_result:
            for pageResults in yearResults:
                for page in pageResults:
                    for row in page:
                        writer.writerow([row,])
    #results = scrapper.scrapeResultsforYears(years,5)
    print(processed_result)
    #
    actualEnd=t.perf_counter()
    print(f'Time Taken to process is {actualEnd - actualStart} sec(s)')
    print("Execution ending at ",dt.datetime.now())


if __name__ == '__main__':
    main()
