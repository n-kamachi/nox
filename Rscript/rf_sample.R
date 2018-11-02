
#パッケージの準備
library(randomForest)#分析用パッケージ
library(inTrees)#結果出力用のパッケージ

#学習用サンプルデータの抽出
num_train<-sample(nrow(iris),100)#学習用データをサンプリング
num_test <-sample(nrow(iris),50)#テスト用データをサンプリング
train_iris<-iris[num_train,]
test_iris <-iris[num_test,] #チューニング
tuneRF(train_iris,train_iris$Species,plot = TRUE, doBest = FALSE)


#分析実行
#mtry（特徴量の数）には、さっきのチューニング結果で「OOB error」の最も小さかった2を利用します。
rf <- randomForest(Species ~., data=train_iris, mtry=2, ntree = 500, predict.all=TRUE)#予測・分類器の構築
result <- predict(rf, newdata=test_iris[-5],predict.all=TRUE)#構築した分類器で、分類対象のデータを分類 
#結果の出力（分類結果の表）#縦軸がテスト用データ、横軸が分析結果
table(test_iris$Species, result$aggregate)


#結果の出力（重要度）
varImpPlot(rf)#重要度のグラフ

rf$importance#重要度をコンソールに出力
treeList$list[1]
treelist2 <- getTree(rf)
#学習後の森構造を要約
treeList <- RF2List(rf)#すべてのtreeの木構造を取得（getTree()する）
exec <- extractRules(treeList,train_iris,ntree=500)#決定木に含まれる条件文を抽出
ruleMetric <- getRuleMetric(exec,train_iris,train_iris$Species)#取り出した条件文を集計
ruleMetric <- pruneRule(ruleMetric,train_iris,train_iris$Species)#余計な条件を削除
ruleMetric <- selectRuleRRF(ruleMetric,train_iris,train_iris$Species)#条件文を集約
learner <- buildLearner(ruleMetric,train_iris,train_iris$Species,minFreq=0.01)#レアな（発生頻度の低い）ルールを切り落とし、1本に集約
readableRules <- presentRules(learner,colnames(train_iris)) #ルールを読みやすく加工
readableRules#結果の表示


