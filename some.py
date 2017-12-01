'''
    各应用商店：获取App的下载量及评论
    大众点评及美团网：餐饮及各类线下门店消费及评价情况
    汽车之家及易车：汽车的相关数据
    58及搜房；房屋租售数据
    新浪微博：用户的各种发言及舆论
    财经数据：雪球及各类财经网站
    宏观数据网站：天气、12306火车、机票网站

    汽车。比如：一年当中买车的最佳时间为何时？ - 何明科的回答，什么样的车可以被称为神车？ - 何明科的回答餐饮。比如：为什么麦当劳和肯德基都开始注重现磨咖啡的推广，其优势与星巴克等传统咖啡行业相比在哪里？ - 何明科的回答消费品。比如：口罩（http://zhuanlan.zhihu.com/hemingke/20391296），尿不湿（http://zhuanlan.zhihu.com/hemingke/20385894）招聘。比如：互联网人士年底怎么找工作（http://zhuanlan.zhihu.com/hemingke/20450600）房地产，这个虐心的行业。比如：深圳的房地产走势（http://zhuanlan.zhihu.com/hemingke/20135185）投融资。比如：用Python抓取投资条款的数据并做NLP以及数据分析：http://zhuanlan.zhihu.com/hemingke/20514731

    完成《building machine learning systems with python》书上的所有projects，这本书除了封面其他里面的内容还是挺实用的。中文书名为  《机器学习系统设计》
    完成kaggle playground和 101上的所有比赛，具体tutorial可以戳

    科赛网相关的东东

'''

# 词频统计和词云的代码
作者：挖数
链接：https://www.zhihu.com/question/28975391/answer/100796070
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

from wordcloud import WordCloud
import jieba
import PIL
import matplotlib.pyplot as plt
import numpy as np

def wordcloudplot(txt):
    path='d:/jieba/msyh.ttf'
    path=unicode(path, 'utf8').encode('gb18030')
    alice_mask = np.array(PIL.Image.open('d:/jieba/she.jpg'))
    wordcloud = WordCloud(font_path=path, 
                          background_color="white",   
                          margin=5, width=1800, height=800,mask=alice_mask,max_words=2000,max_font_size=60,random_state=42) 
    wordcloud = wordcloud.generate(txt)
    wordcloud.to_file('d:/jieba/she2.jpg')
    plt.imshow(wordcloud)
    plt.axis("off")
    plt.show()
    
def main():
    a=[]
    f=open(r'd:\jieba\book\she.txt','r').read()
    words=list(jieba.cut(f))
    for word in words:
        if len(word)>1:
            a.append(word)
    txt=r' '.join(a)
    wordcloudplot(txt)
    
if __name__=='__main__':
    main()

# ---

# ---爬知乎女神的代码--- #
作者：挖数
链接：https://www.zhihu.com/question/28975391/answer/100796070
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

import requests
import urllib
import re
import random
from time import sleep

def main():
    url='xxx'
    headers={xxx}
    i=925
    for x in xrange(1020,2000,20):
        data={'start':'1000',
    'offset':str(x),
    '_xsrf':'a128464ef225a69348cef94c38f4e428'}
        content=requests.post(url,headers=headers,data=data,timeout=10).text
        imgs=re.findall('<img src=\\\\\"(.*?)_m.jpg',content)    
        for img in imgs:
            try:
                img=img.replace('\\','')
                pic=img+'.jpg'
                path='d:\\bs4\\zhihu\\jpg4\\'+str(i)+'.jpg'
                urllib.urlretrieve(pic,path)
                print ('下载了第'+str(i)+u'张图片')
                i+=1
                sleep(random.uniform(0.5,1))
            except:
                print ('抓漏1张')
                pass
        sleep(random.uniform(0.5,1))
        
if __name__=='__main__':
    main()


# -----------------------
# 一个数据分析
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import * 
np.random.seed(123) #设置随机种子

iris = pd.read_csv("c:\\Users\\Administrator\\Destop\\iris.csv", header = False)
print(iris.shape)
print(isis.head())

print(iris.describe().T)

irisK3 = cluster.KMeans(n_clusters = 3, random_state = 1)
irisFeatures = iris.ix[:, 1:4]
print(irisFeatures.head())
irisK3.fit(irisFeatures)
print(irisK3.labels_)


# 决策树
target = iris['Name']
data = iris.ix[:, 1:4]
train_data, test_data, train_target, test_target = cross_validation.train_test_split(data,
                        target, test_size = 0.24, random_state =0)

clf = tree.DecisionTreeClassifer(criterion = 'gini', max_depth = 6,
                                 min_samples_split = 5)

clf_fit = clf.fit(train_data, train_target)

train_est = clf.predict(train_data)
test_est = clf.predict(test_data)

sum = 0
for i in range(36):
    if test_est[i] == test_target[i]:
        sum = sum + 1

print('test_accuracy=', "%.2f%%" %(sum*1.0/36*100))


# ---王者荣耀-NBA数据分析--- #
# coding:utf-8
# import base tools
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import style
import warnings
warnings.filterwarnings('ignore')
style.use('ggplot')
% matplotlib inline

# 读取季后赛和常规赛数据
team_playoff = pd.read_csv('../input/NBAdata/team_playoff.csv')
team_season = pd.read_csv('../input/NBAdata/team_season.csv')

# split season apart 自定义函数，整合出赛季标签便于后面分组；构造分差数据新特征
def add_season(date):
    season = np.zeros(len(date))
    for i,d in enumerate(date.str.split('-').str[0:2]):
        season[i] = int(d[0]) if int(d[1])<7 else int(d[0])+1
    return season

# generate score_diff_team_a = score(team_a) - score(team_b)
def add_score_diff(res):
    diff = np.zeros(len(res))
    for i,d in enumerate(res.str.split('-')):
        diff[i] = abs(int(d[1][0:-3]) - int(d[0][3:]))
    return diff

team_season['分差'] = add_score_diff(team_season['比分'])
team_playoff['分差'] = add_score_diff(team_playoff['比分'])
team_season['赛季'] = add_season(team_season.时间)
team_playoff['赛季'] = add_season(team_playoff.时间)

# replace ['L'/'W'] with [-1/1]
team_season['结果'].replace(['L','W'],[-1,1], inplace=True)
team_playoff['结果'].replace(['L','W'],[-1,1], inplace=True)

# ---常规赛
# 常规赛分析
temp = team_season[['得分','赛季','分差','篮板','犯规','罚球','罚球命中','罚球出手','失误','助攻','投篮']].groupby('赛季')
plt.figure(figsize=(14,4))
plt.subplot(121)
plt.title('各个赛季得分: 极值与均值')
plt.plot(temp['得分'].min(),'g.',alpha=0.7)
plt.plot(temp['得分'].max(),'g.',alpha=0.7)
plt.plot(temp['得分'].mean(),'bo',temp.mean()['得分'],'k',alpha=0.8)
plt.subplot(122)
plt.title('各个赛季得分标准差与场均分差')
plt.plot(temp['得分'].std(),'go',temp['得分'].std(),'k',alpha=0.8)
plt.plot(temp['分差'].mean(),'ro',temp['分差'].mean(),'k',alpha=0.8)
plt.legend()

plt.figure(figsize=(14,4))
plt.subplot(131)
plt.plot(temp.min()['篮板'],'g.',alpha=0.7)
plt.plot(temp.max()['篮板'],'g.',alpha=0.7)
plt.plot(temp.mean()['篮板'],'o',temp.mean()['篮板'],'k',alpha=0.8)

plt.subplot(231)
plt.plot(temp['失误'].mean(),'ko',temp['失误'].mean(),'grey',alpha=0.8);plt.legend()
plt.subplot(234)
plt.plot(temp['助攻'].mean(),'yo',temp['助攻'].mean(),'c',alpha=0.8);plt.legend()
plt.subplot(232)
plt.plot(temp['犯规'].mean(),'co',temp['犯规'].mean(),'k',alpha=0.8);plt.legend()
plt.subplot(235)
plt.plot(temp['罚球'].mean(), 'o',temp['罚球'].mean(),'grey',alpha=0.8);plt.legend()
plt.subplot(233)
plt.plot(temp['投篮'].mean(),'ro',temp['投篮'].mean(),'k',alpha=0.8);plt.legend()
plt.subplot(236)
plt.plot(temp['篮板'].mean(),'ko',temp['篮板'].mean(),'orange',alpha=0.8);plt.legend()

# --- 季后赛
# 季后赛分析
temp = team_playoff[['得分','赛季','分差','篮板','犯规','罚球','罚球命中','罚球出手','失误','助攻','投篮']].groupby('赛季')
plt.figure(figsize=(14,4))
plt.subplot(121)
plt.title('各个赛季得分: 极值与均值')
plt.plot(temp['得分'].min(),'g.',alpha=0.7)
plt.plot(temp['得分'].max(),'g.',alpha=0.7)
plt.plot(temp['得分'].mean(),'bo',temp.mean()['得分'],'k',alpha=0.8)
plt.subplot(122)
plt.title('各个赛季得分标准差与场均分差')
plt.plot(temp['得分'].std(),'go',temp['得分'].std(),'k',alpha=0.8)
plt.plot(temp['分差'].mean(),'ro',temp['分差'].mean(),'k',alpha=0.8)
plt.legend()

plt.figure(figsize=(14,4))
plt.subplot(131)
plt.plot(temp.min()['篮板'],'g.',alpha=0.7)
plt.plot(temp.max()['篮板'],'g.',alpha=0.7)
plt.plot(temp.mean()['篮板'],'o',temp.mean()['篮板'],'k',alpha=0.8)

plt.subplot(231)
plt.plot(temp['失误'].mean(),'ko',temp['失误'].mean(),'grey',alpha=0.8);plt.legend()
plt.subplot(234)
plt.plot(temp['助攻'].mean(),'yo',temp['助攻'].mean(),'c',alpha=0.8);plt.legend()
plt.subplot(232)
plt.plot(temp['犯规'].mean(),'co',temp['犯规'].mean(),'k',alpha=0.8);plt.legend()
plt.subplot(235)
plt.plot(temp['罚球'].mean(), 'o',temp['罚球'].mean(),'grey',alpha=0.8);plt.legend()
plt.subplot(233)
plt.plot(temp['投篮'].mean(),'ro',temp['投篮'].mean(),'k',alpha=0.8);plt.legend()
plt.subplot(236)
plt.plot(temp['篮板'].mean(),'ko',temp['篮板'].mean(),'orange',alpha=0.8);plt.legend()

# ---梅西法排序得分
team_playoff['分差'] *= team_playoff['结果']
team_season['分差'] *= team_playoff['结果']

def Messi_rank(season_data, index):
    train = season_data[['球队',index]].groupby('球队').sum()
    A = -np.ones((train.shape[0],train.shape[0]))
    for i in range(0,train.shape[0]-1):
        A[i,i] += train.shape[0]
    A[i+1] = 1
    train.iloc[-1] = 0
    rank = np.dot(np.linalg.inv(A),train.values)
    rank = pd.DataFrame(rank,index=train.index)
    return rank

group_playoff = team_playoff.groupby('赛季')
messi_res = []
for s,d in group_playoff:
    messi_res.append(Messi_rank(d,'分差'))

fig, axes = plt.subplots(nrows=2, ncols=3,figsize=(16,8))
r = c = 0
for i,d in enumerate(messi_res):
    if(i+1986 in [1996,1997,1998,2001,2014,2016]):
        d.sort_values(by=0).plot(kind='bar',ax=axes[r,c],title=i+1986,label=None,legend=None)
        c += 1
        if(c==3):
            c = 0;r=1


# 导入部分我喜欢用的机器学习库
from sklearn.linear_model import LogisticRegression as LR
from sklearn.svm import SVC
from sklearn import preprocessing

data = team_playoff.groupby(['赛季','球队'])
features = team_playoff.columns[5:-1]
print('Simple Features: \n\t',features)

gtrain_data = data.mean()[features].reset_index()
winner_count = data.结果.sum()
winner_count = winner_count.reset_index()
winner_count.head()

# 建立历年冠军index与冠军数据集
champs = None
champs_index = winner_count.groupby('赛季').apply(lambda t: t[t['结果']==t['结果'].max()])
champs_index = champs_index.drop([(2013.0,411)]) # 数据有些问题，根据净胜结果选取冠军同时出现了2013马刺和热火，因此删除
champs = team_playoff.merge(champs_index, on=['赛季','球队'],how='inner').groupby(['赛季','球队']).mean()[features]

finals = []
features_importance = []
# 记录最终结果和特征重要性(模型内参数)
for season,tr in gtrain_data.groupby('赛季'):
    tr = tr[features]
    score = winner_count[winner_count['赛季']==season]['结果']
    score = (score==max(score))*1
    model1 = LR()
    model1.fit(tr,score)
    features_importance.append(model1.coef_[0])
    finals.append(model1.predict_proba(champs)[:,1])
    print(season,',',model1.score(tr,score),end='| ')
    if(season%5==0): print('\n')

# 最终获得的概率输出如图所示
cols = champs_index['赛季'].astype(str).str[0:4] + '-' + champs_index['球队']
finals = pd.DataFrame(finals)
finals.columns = cols.values

ranks = []
for name,row in finals.iterrows():
    row = np.array(row)
    row[np.argsort(row)] = range(0,len(row))
    ranks.append(row)
ranks = pd.DataFrame(ranks, columns=finals.columns)
ranks.sum().sort_values().plot(kind='barh',figsize=(10,6),color='darkred')

# 这里使用seaborn来绘制一个colormap 每个模型的对球队的概率输出
plt.figure(figsize=(15,10))
_,ax = plt.subplots()
ax.set_xticks(np.arange(ranks.shape[1])+0.5, minor=False)
ax.set_yticks(np.arange(ranks.shape[0])+0.5, minor=False)
ax.pcolor(ranks.T,cmap=plt.cm.Purples, alpha=0.8)
ax.set_xticklabels(ranks.T.columns, minor=False)
ax.set_yticklabels(ranks.T.index, minor=False)



features_importance = pd.DataFrame(features_importance,columns = features)
features_importance.index += 1986
features_importance

features_importance.mean().plot(kind='bar',figsize=(16,8))



# ------------------------------------------------------------------ #
# 科乔丹对比分析
# 常规数据分析：


import pandas as pd
from pandas import Series,DataFrame

data = {'avg_score':[30.28,25.00,27.13,22.80],'rebound':[6.20,5.24,7.26,4.36],'offensive_rebound':[1.51,1.11,1.21,0.71],'defensive_rebound':[4.68,4.12,6.05,3.65],
        'assist':[5.20,4.68,7.03,6.82],'steal':[2.34,1.44,1.65,1.79],'block':[0.83,0.48,0.77,0.21],'starting':[0.97,0.89,1.00,0.98],'avg_time':[38.25,36.14,38.92,34.67],'hit_rate':[0.49,0.44,0.50,0.47],
        'total_shoot/avg_shoot':['22912/23.14','26200/19.47','20803/19.61','9645/16.80'],'total_hit/avg_hit':['11355/11.46','11719/8.71','10423/9.82','4589/8.00'],
        '3ball_hitrate':[0.28,0.30,0.31,0.42],'3ball_total_hit/3ball_avg_hit':['572/0.58','1827/1.36','1467/1.38','1917/3.34'],
        '3ball_total_shoot/3ball_avg_shoot':['1726/1.74','5546/4.12','4295/4.05','4370/7.63'],
        'freethrow_hitrate':[0.82,0.83,0.74,0.89],
        'fault':[2.66,2.98,3.41,3.17],'foul':[2.52,2.49,1.86,2.51]}

frame = DataFrame(data, columns = ['avg_score','rebound','offensive_rebound','defensive_rebound','assist','steal','block','starting','avg_time','hit_rate',
                                   'total_shoot/avg_shoot','3ball_hitrate','3ball_total_hit/3ball_avg_hit','3ball_total_shoot/3ball_avg_shoot',
                                   'freethrow_hitrate','fault','foul'],index = ['Jordan','Kobe','Lebron','Curry'])
frame

# -84-85赛季到16-17赛季巨星场均得分分析- #
import pandas as pd
from pandas import DataFrame

data = {'84-85':[28.2,0,0,0],'85-86':[22.7,0,0,0],'86-87':[37.1,0,0,0],
        '87-88':[35.0,0,0,0],'88-89':[32.5,0,0,0],'89-90':[33.6,0,0,0],'90-91':[31.5,0,0,0],
        '91-92':[30.1,0,0,0],'92-93':[32.6,0,0,0],'93-94':[0,0,0,0],
        '94-95':[26.9,0,0,0],'95-96':[30.4,0,0,0],'96-97':[29.6,7.6,0,0],
        '97-98':[28.7,15.4,0,0],'98-99':[0,19.9,0,0],'99-00':[0,22.5,0,0],
        '00-01':[0,28.5,0,0],'01-02':[22.9,25.2,0,0],'02-03':[20.0,30.0,0,0],
        '03-04':[0,24.0,20.9,0],'04-05':[0,27.6,27.2,0],'05-06':[0,35.4,31.2,0],
        '06-07':[0,31.6,27.3,0],'07-08':[0,28.3,30.0,0],'08-09':[0,26.8,28.4,0],
        '09-10':[0,27.0,29.7,17.5],'10-11':[0,25.3,26.7,18.6],'11-12':[0,27.9,27.1,14.7],
        '12-13':[0,27.3,26.8,22.9],'13-14':[0,13.8,27.1,24.0],'14-15':[0,22.3,25.3,23.8],
        '15-16':[0,17.6,25.3,30.1],'16-17':[0,0,26.4,25.3]  }

frame2 = pd.DataFrame(data,columns = ['84-85','85-86','86-87','87-88','88-89','89-90','90-91','91-92','92-93','93-94',
                                        '94-95','95-96','96-97','97-98','98-99','99-00','00-01','01-02','02-03','03-04','04-05','05-06','06-07','07-08','08-09',
                                        '09-10','10-11','11-12','12-13','13-14','14-15','15-16','16-17'],index = ['jordan','kobe','lebron','curry'])
frame2


import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter,MaxNLocator
import sys
%matplotlib inline
x = frame2.columns
x_list = range(len(x))
y_1 = frame2.ix[0]
y_2 = frame2.ix[1]
y_3 = frame2.ix[2]
y_4 = frame2.ix[3]

plt.figure(figsize = (20,8))
plt.xlabel('year')
plt.ylabel('score')
plt.plot(x_list, y_1, 'r',label = "jordan")
plt.plot(x_list, y_2, 'b',label = 'kobe')
plt.plot(x_list, y_3, 'y',label = 'lebron')
plt.plot(x_list, y_4, 'g',label = 'curry')
plt.legend( loc = 'upper right') 
plt.xticks(x_list,x)
plt.show()

# 直观给出四位巨星的生涯场均得分分布
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pandas import Series,DataFrame
from matplotlib.ticker import FuncFormatter,MaxNLocator
import seaborn as sns
from pylab import *

%matplotlib inline
names = frame.index
x = range(len(names))
y = frame['avg_score']
plt.figure(figsize=(10, 6))
plt.xlabel('Player')
plt.ylabel('Score')
plt.title('Score distribution')
plt.plot(x,y,'')
plt.xticks(x,names)
plt.show()

# 这部分将对四位巨星的得分分布以表格的形式进行展现，使读者对各位巨星的生涯得分分布有个大致印象

import csv
import math
import matplotlib.pyplot as plt
from pandas import Series,DataFrame
with open ('../input/NBAdata/player_season.csv','r') as csvfile:
    reader = csv.reader(csvfile)
    jordan_score = [row[24] for row in reader if 'Michael Jordan' in row]
with open ('../input/NBAdata/player_season.csv','r') as csvfile1:
    reader = csv.reader(csvfile1)
    kobe_score = [row[24] for row in reader if 'Kobe Bryant' in row]
with open ('../input/NBAdata/player_season.csv','r') as csvfile2:
    reader = csv.reader(csvfile2)
    lebron_score = [row[24] for row in reader if 'LeBron James' in row]
with open ('../input/NBAdata/player_season.csv','r') as csvfile3:
    reader = csv.reader(csvfile3)
    curry_score = [row[24] for row in reader if 'Stephen Curry' in row]
    
jordan_score_process =set(jordan_score)
jordan_set1 = []
jordan_set2 = []
jordan_set3 = []
jordan_set4 = []

kobe_set1 = []
kobe_set2 = []
kobe_set3 = []
kobe_set4 = []

lebron_set1 = []
lebron_set2 = []
lebron_set3 = []
lebron_set4 = []

curry_set1 = []
curry_set2 = []
curry_set3 = []
curry_set4 = []


for i in jordan_score:
    if round(float(i)) < 20:
        jordan_set1.append(i)
    if 20 <= round(float(i)) < 40:
        jordan_set2.append(i)
    if 40 <= round(float(i)) < 60:
        jordan_set3.append(i)
    if round(float(i)) >= 60:
        jordan_set4.append(i)


for i in kobe_score:
    if round(float(i)) < 20:
        kobe_set1.append(i)
    if 20 <= round(float(i)) < 40:
        kobe_set2.append(i)
    if 40 <= round(float(i)) < 60:
        kobe_set3.append(i)
    if round(float(i)) >= 60:
        kobe_set4.append(i)
        
for i in lebron_score:
    if round(float(i)) < 20:
        lebron_set1.append(i)
    if 20 <= round(float(i)) < 40:
        lebron_set2.append(i)
    if 40 <= round(float(i)) < 60:
        lebron_set3.append(i)
    if round(float(i)) >= 60:
        lebron_set4.append(i)
        

for i in curry_score:
    if round(float(i)) < 20:
        curry_set1.append(i)
    if 20 <= round(float(i)) < 40:
        curry_set2.append(i)
    if 40 <= round(float(i)) < 60:
        curry_set3.append(i)
    if round(float(i)) >= 60:
        curry_set4.append(i)
        
data = {'jordan':[len(jordan_set1),len(jordan_set2),len(jordan_set3),len(jordan_set4)],
        'kobe':[len(kobe_set1),len(kobe_set2),len(kobe_set3),len(kobe_set4)],
        'lebron':[len(lebron_set1),len(lebron_set2),len(lebron_set3),len(lebron_set4)],
        'curry':[len(curry_set1),len(curry_set2),len(curry_set3),len(curry_set4)]}
frame = DataFrame(data, columns = ['jordan','kobe','lebron','curry'],
                  index = ['score <= 20','20<score<=40','40<score<=60','score>60'])
frame

# 该部分是Jordan的生涯得分分布，分为四个部分
plt.figure(figsize=(7,7))
colors = ['red', 'yellow', 'blue', 'green']
explode = (0.05,0,0,0)
jordan_score_set = []
jordan_score_set.append(len(jordan_set1))
jordan_score_set.append(len(jordan_set2))
jordan_score_set.append(len(jordan_set3))
jordan_score_set.append(len(jordan_set4))
jordan_score_labels = ['<20','20~40','40~60','>60']
plt.pie(jordan_score_set, explode = explode, labels= jordan_score_labels, colors= colors, labeldistance=1.1,
                autopct='%5.00f%%', shadow=False, startangle=90, pctdistance= 0.8)
plt.axis('equal')
plt.title("jordan's score distribution")
plt.grid()
plt.show()

# 该部分是kobe的生涯得分分布，分为四个部分
plt.figure(figsize=(7,7))
colors = ['red', 'yellow', 'blue', 'green']
explode = (0.05,0,0,0)
kobe_score_set = []
kobe_score_set.append(len(kobe_set1))
kobe_score_set.append(len(kobe_set2))
kobe_score_set.append(len(kobe_set3))
kobe_score_set.append(len(kobe_set4))
kobe_score_labels = ['<20','20~40','40~60','>60']
plt.pie(kobe_score_set, explode = explode, labels= kobe_score_labels, colors= colors, labeldistance=1.1,
                autopct='%5.00f%%', shadow=False, startangle=90, pctdistance= 0.8)
plt.axis('equal')
plt.title("kobe's score distribution")
plt.grid()
plt.show()


# 该部分是lebron的生涯得分分布，分为四个部分
plt.figure(figsize=(7,7))
colors = ['red', 'yellow', 'blue', 'green']
explode = (0.05,0,0,0)
lebron_score_set = []
lebron_score_set.append(len(lebron_set1))
lebron_score_set.append(len(lebron_set2))
lebron_score_set.append(len(lebron_set3))
lebron_score_set.append(len(lebron_set4))
lebron_score_labels = ['<20','20~40','40~60','>60']
plt.pie(lebron_score_set, explode = explode, labels= lebron_score_labels, colors= colors, labeldistance=1.1,
                autopct='%5.00f%%', shadow=False, startangle=90, pctdistance= 0.8)
plt.axis('equal')
plt.title("lebron's score distribution")
plt.grid()
plt.show()

# 该部分是的生涯得分分布，分为四个部分
plt.figure(figsize=(7,7))
colors = ['red', 'yellow', 'blue', 'green']
explode = (0.05,0,0,0)
curry_score_set = []
curry_score_set.append(len(curry_set1))
curry_score_set.append(len(curry_set2))
curry_score_set.append(len(curry_set3))
curry_score_set.append(len(curry_set4))
curry_score_labels = ['<20','20~40','40~60','>60']
plt.pie(curry_score_set, explode = explode, labels= curry_score_labels, colors= colors, labeldistance=1.1,
                autopct='%5.00f%%', shadow=False, startangle=90, pctdistance= 0.8)
plt.axis('equal')
plt.title("curry's score distribution")
plt.grid()
plt.show()


# --场均得分分析-- #
# -- 季后赛表现 -#
import pandas as pd
from pandas import Series,DataFrame

data = {'avg_score':[33.54,25.64,28.40,26.20],'rebound':[6.45,5.09,8.86,5.09],'offensive_rebound':[1.70,1.05,1.54,0.88],'defensive_rebound':[4.75,4.04,7.32,4.21]
,'assist':[5.65,4.73,6.86,6.64],'steal':[2.09,1.41,1.79,1.76],'block':[0.88,0.65,0.96,0.20],'avg_time':[41.73,39.28,42.09,37.85],'hit_rate':[0.49,0.45,0.49,0.45],
'avg_shoot':[25.25,20.45,20.72,19.20],'3ball_hitrate':[0.32,0.31,0.31,0.41],'3ball_avg_shoot':[2.50,4.01,4.62,10.20],'freethrow_hitrate':[0.82,0.80,0.73,0.91],
'freethrow_pergame':[9.76,7.35,9.12,5.23],'fault':[3.03,2.94,3.56,3.76],'foul':[3.01,3.00,2.38,2.35]}

frame = DataFrame(data,columns = ['avg_score','rebound','offensive_rebound','defensive_rebound','assist','steal','block','avg_time','hit_rate',
                                   'avg_shoot','3ball_hitrate','3ball_avg_shoot',
                                   'freethrow_hitrate','freethrow_pergame','fault','foul'],index = ['Jordan','Kobe','Lebron','Curry'] )
frame

# 首先是一个各个球员在常规赛和季后赛的一个得分比较。

# Jordan：
import csv
with open ('../input/NBAdata/avg.csv','r') as csvfile:
    reader = csv.reader(csvfile)
#     jordan_year = [row[1] for row in reader if 'Michael Jordan' in row]
    jordan_score = [row[23] for row in reader if 'Michael Jordan' in row]
    
with open ('../input/NBAdata/avg.csv','r') as csvfile1:
    reader = csv.reader(csvfile1)
    kobe_year = [row[1] for row in reader if 'Kobe Bryant' in row]
    kobe_score = [row[23] for row in reader if 'Kobe Bryant' in row]
    
with open ('../input/NBAdata/avg.csv','r') as csvfile2:
    reader = csv.reader(csvfile2)
#     lebron_year = [row[1] for row in reader if 'LeBron James' in row]
    lebron_score = [row[23] for row in reader if 'LeBron James' in row]
    
with open ('../input/NBAdata/avg.csv','r') as csvfile3:
    reader = csv.reader(csvfile3)
#     curry_year = [row[1] for row in reader if 'Stephen Curry' in row]
    curry_score = [row[23] for row in reader if 'Stephen Curry' in row]

with open ('../input/NBAdata/player_playoff.csv','r') as csvfile4:
    reader = csv.reader(csvfile4)
    curry_playoff_year = [row[1] for row in reader if 'Stephen Curry' in row]
    curry_playoff_score = [row[2] for row in reader if 'Stephen Curry' in row]
    
data = {'84-85':[28.2,0,0,0],'85-86':[22.7,0,0,0],'86-87':[37.1,0,0,0],
        '87-88':[35.0,0,0,0],'88-89':[32.5,0,0,0],'90-91':[31.5,0,0,0],
        '91-92':[30.1,0,0,0],'92-93':[32.6,0,0,0],'93-94':[0,0,0,0],
        '94-95':[26.9,0,0,0],'95-96':[30.4,0,0,0],'96-97':[29.6,7.6,0,0],
        '97-98':[28.7,15.4,0,0],'98-99':[0,19.9,0,0],'99-00':[0,22.5,0,0],
        '00-01':[0,28.5,0,0],'01-02':[22.9,25.2,0,0],'02-03':[20.0,30.0,0,0],
        '03-04':[0,24.0,20.9,0],'04-05':[0,27.6,27.2,0],'05-06':[0,35.4,31.2,0],
        '06-07':[0,31.6,27.3,0],'07-08':[0,28.3,30.0,0],'08-09':[0,26.8,28.4,0],
        '09-10':[0,27.0,29.7,17.5],'10-11':[0,25.3,26.7,18.6],'11-12':[0,27.9,27.1,14.7],
        '12-13':[0,27.3,26.8,22.9],'13-14':[0,13.8,27.1,24.0],'14-15':[0,22.3,25.3,23.8],
        '15-16':[0,17.6,25.3,30.1],'16-17':[0,0,26.4,25.3]  }

jordan_score
# 以上是对四位巨星的常规赛平均得分的获取，接下来是对各位球星季后赛得分的获取，因为给的表格中貌似没有季后赛球员得分（如果有的话请告知），所以就不能直接获取了，
# 计算过程比较繁琐，故直接给出计算后的各个球员季后赛平均得分：
# kobe_playoff_score = []

import csv
import math
import matplotlib.pyplot as plt
from pandas import Series,DataFrame
with open ('../input/NBAdata/player_playoff.csv','r') as csvfile:
    reader = csv.reader(csvfile)
    jordan_score = [row[24] for row in reader if 'Michael Jordan' in row]
with open ('../input/NBAdata/player_season.csv','r') as csvfile1:
    reader = csv.reader(csvfile1)
    kobe_score = [row[24] for row in reader if 'Kobe Bryant' in row]
with open ('../input/NBAdata/player_season.csv','r') as csvfile2:
    reader = csv.reader(csvfile2)
    lebron_score = [row[24] for row in reader if 'LeBron James' in row]
with open ('../input/NBAdata/player_season.csv','r') as csvfile3:
    reader = csv.reader(csvfile3)
    curry_score = [row[24] for row in reader if 'Stephen Curry' in row]
    


# --- 版本二 --- #
# 导入必要的包.
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import warnings
import numpy as np
import pylab

%matplotlib inline
warnings.filterwarnings('ignore')

player_avg = pd.read_csv('avg.csv')  player_avg.head()

pd.set_option('display.max_columns',30)
player_avg[(player_avg['姓名'] == 'Kawhi Leonard') & (player_avg['赛季'] == '16--17')]

pd.set_option('display.max_columns',50)
L_1617_avg = player_avg[(player_avg['姓名'] == 'LeBron James') & (player_avg['赛季'] == '16--17')]
L_1617_avg

作者：科赛网Kesci
链接：https://www.zhihu.com/question/28975391/answer/231489176
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

class Radar(object):
    n = 1
    angles =None
    def __init__(self, fig, titles, labels, rect=None):
        if rect is None:
            rect = [0.05, 0.05, 0.95, 0.95]

        self.n = len(titles)
        self.angles = np.arange(90, 90+360, 360.0/self.n)
        self.axes = [fig.add_axes(rect, projection="polar", label="axes%d" % i) 
                         for i in range(self.n)]

        self.ax = self.axes[0]
        self.ax.set_thetagrids(self.angles, labels=titles, fontsize=14)

        for ax in self.axes[1:]:
            ax.patch.set_visible(False)
            ax.grid("off")
            ax.xaxis.set_visible(False)

        for ax, angle, label in zip(self.axes, self.angles, labels):
            ax.set_rgrids(range(1, 6), angle=angle, labels=label)
            ax.spines["polar"].set_visible(False)
            ax.set_ylim(0, 5)

    def angle(self, values, *args, **kw):
        return np.deg2rad(np.r_[self.angles]),np.r_[values],values

    def plot(self, values, *args, **kw):
        angle = np.deg2rad(np.r_[self.angles, self.angles[0]])
        values = np.r_[values, values[0]]
        self.ax.plot(angle, values, *args, **kw)
titles_ = ['score','shoot','rebound','assist','three','penalty','steal','block']
titles = ['得分','投篮','篮板','助攻','三分','罚球','抢断','盖帽']
# titles = list("ABCDE")


# 角度1：基于篮板球

# data_statistics函数主要是方便categoricl型的数据的统计显示，方便后续绘图使用
def data_statistics(Kawhi_season1617, Lebron_season1617, name):
    Kawhi_season1617_ = pd.DataFrame(Kawhi_season1617.groupby(name)['球员'].count())
    Kawhi_season1617_.columns = ['K_次数']
    Kawhi_season1617_.reset_index(inplace=True) 

    Lebron_season1617_ = pd.DataFrame(Lebron_season1617.groupby(name)['球员'].count())
    Lebron_season1617_.columns = ['L_次数']
    Lebron_season1617_.reset_index(inplace=True)

    data = pd.merge(Lebron_season1617_,Kawhi_season1617_,on = name , how ='outer')
    data = data.fillna(0)
    data = data.sort_values(name) 
    return data


rebounds['K_次数'] =  rebounds['K_次数'] * -1
plt.figure(figsize=[16,6])
sns.barplot(x = '篮板', y = 'L_次数', data = rebounds, color='red')
sns.barplot(x = '篮板',y = 'K_次数', data = rebounds, color ='blue')

# --- 角度2：基于得分 --- #
score['K_次数'] =  score['K_次数'] * -1
plt.figure(figsize=[16,6])
sns.barplot(x = '得分', y = 'L_次数', data = score, color='red')
sns.barplot(x = '得分',y = 'K_次数', data = score, color ='blue')

plt.figure(figsize= [12,5])
total = pd.concat([Kawhi_season1617,Lebron_season1617])
total['Is_Kawhi'] = 0
total.loc[total['球员'] == 'Kawhi Leonard','Is_Kawhi'] = 1
total['A'] = 0
sns.violinplot(x= 'A' , y = '得分', hue = 'Is_Kawhi', data = total, split=True)

# ---角度4：基于抢断和盖帽的分析--- #
steal = data_statistics(Kawhi_season1617, Lebron_season1617, '抢断')
steal
steal['K_次数'] =  steal['K_次数'] * -1
plt.figure(figsize=[16,6])
sns.barplot(x = '抢断', y = 'L_次数', data = steal, color='red')
sns.barplot(x = '抢断',y = 'K_次数', data = steal, color ='blue')

block = data_statistics(Kawhi_season1617, Lebron_season1617, '盖帽')
block

block['K_次数'] =  block['K_次数'] * -1
plt.figure(figsize=[16,6])
sns.barplot(x = '盖帽', y = 'L_次数', data = block, color='red')
sns.barplot(x = '盖帽',y = 'K_次数', data = block, color ='blue')

# ---角度5：基于失误次数的分析--- #
fault_num = data_statistics(Kawhi_season1617, Lebron_season1617, '失误')
fault_num
fault_num['K_次数'] =  fault_num['K_次数'] * -1
plt.figure(figsize=[16,6])
sns.barplot(x = '失误', y = 'L_次数', data = fault_num, color='red')
sns.barplot(x = '失误',y = 'K_次数', data = fault_num, color ='blue')

