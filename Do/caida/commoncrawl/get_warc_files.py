import subprocess
import parseWarc
import json
import codecs

def get_paths(year, week, outfile):
    #file to write json to
    f = open(outfile, 'w')

    #appends 0 to front of week if <10
    #puts in format ##
    if week < 10:
       str_week = '0' + str(week)
    else:
       str_week = str(week)
    
    #constructs the url to the file that will contain references
    #to each section in that weeks crawl
    path = "https://commoncrawl.s3.amazonaws.com/crawl-data/CC-MAIN-" + \
           str(year) + '-' + str_week + "/warc.paths.gz"

    print("geting paths")

    # retreves warc_paths.gz file that contines the path to each section
    process = subprocess.Popen(['curl', '-o', 'warc_paths.gz', path])
    process.communicate()

    #unzips the warc_paths.gz file 
    process = subprocess.Popen(['gunzip', 'warc_paths.gz'])
    process.communicate()
    
    #opens warc_paths with each line being a path to crwal section
    warc_paths = codecs.open("warc_paths", 'r', encoding='utf-8',errors='ignore') 

    #path number identifies which section is being worked on
    #for debuging purpuses only
    path_num = 1

    #for each section in warc_paths download the file
    # prosses it into a json object then write to outfile
    for warc_path in warc_paths:
        print("geting warc data for path " + str(path_num))

        #url where crawl secion is
        warc_url = "https://commoncrawl.s3.amazonaws.com/" + warc_path.strip()
        #download crawl section into warc_data.gz
        process = subprocess.Popen(['curl', '-o', 'warc_data.gz', warc_url])
        process.communicate()
 
        #unzips warc_data.gz
        process = subprocess.Popen(['gunzip', 'warc_data.gz'])
        process.communicate()
       
        ############################################################################
        #used for debuging purpuses creast a file and then copy a small section of
        #warc data into it to run faster tests.
        #process = subprocess.Popen(['touch', 'sub_warc_data'])
        #process.communicate()
        #print('copying subset of data to be parsed')
        #data = codecs.open('warc_data', 'r', encoding='utf-8',errors='ignore')
        #short = open('sub_warc_data', 'w')
        #for i in range(10000000):
        #    short.write(data.readline())
        #data.close()
        #short.close()
        ###########################################################################
        
        print('parsing data')

        #parces data into a dictionary that maps each ip
        #address onto its corrisponding url
        dic = parseWarc.ip_url_from_warc("warc_data")
 
        #writes to json file
        json.dump(dic, f)
  
        #removes warc_data so when next download happends
        #user does not have to manualy instruct curl to 
        #over write warc_data
        process = subprocess.Popen(['rm', 'warc_data']) 
        #process = subprocess.Popen(['rm', 'sub_warc_data']) 
        process.communicate()
        break
 
    warc_paths.close()
    f.close()

    #removes warc_paths when all paths have been
    #downloaded and parsed
    process = subprocess.Popen(['rm', 'warc_paths']) 
    process.communicate()
