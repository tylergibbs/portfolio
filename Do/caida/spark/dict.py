from pyspark import SparkContext
sc = SparkContext()

words = sc.textFile("dictionary")

print("")
print(words.count())
print("")

words2 = words.map(lambda x: [x[0:(len(x)//2)], x[len(x)//2:len(x)]])

print("")
print(words2.count())
print("")

