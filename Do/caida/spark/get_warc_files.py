from pyspark import SparkContext
from parse_warc import get_ip_url_dict
import wandio


def get_paths(year, week, outfile):
    sc = SparkContext()

    #appends 0 to front of week if <10
    #puts in format ##
    str_week = "{:02d}".format(week)
    str_year = str(year)

    #constructs the url to the file that will contain references
    #to each section in that weeks crawl
    path = "https://commoncrawl.s3.amazonaws.com/crawl-data/CC-MAIN-" + \
           str_year + '-' + str_week + "/warc.paths.gz"

    print("geting paths")

    # retreves warc_paths.gz file that contines the path to each section
    with wandio.open(path) as warc_paths_single:

       print(warc_paths_single_small)

       #opens warc_paths with each line being a path to crwal section
       warc_paths = sc.parallelize(warc_paths_single)

       ################################################################
       def parse_section(warc_path):
        
          #url where crawl secion is
          warc_url = "https://commoncrawl.s3.amazonaws.com/" + warc_path.strip()
          #download crawl section into warc_data.gz
          with wandio.open(warc_url) as warc_file:

               print('parsing data')

               #parces data into a dictionary that maps each ip
               #address onto its corrisponding url
               pairs = get_ip_url_dict(warc_file)
               
               return pairs

       #for each section in warc_paths download the file
       # prosses it into a json object then write to outfile
       ip_url_RDD = warc_paths.flatMap(parse_section)\
                              .reduceByKey(lambda x, y: x | y)\
                              .map(lambda x: x[0] + ' | ' + ', '.join(x[1]))

       ip_url_RDD.saveAsTextFile(outfile) 

get_paths(2018, 5, 'out')
