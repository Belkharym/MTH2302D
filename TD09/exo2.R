### A)

N = 10
ALPHA = 1 - TRUST_THRESHOLD
qt(1 - ALPHA / 2, n - 1)

### B)

# 1)

intervalle_confiance = read.csv("intervalle_confiance.csv", header = TRUE, sep = ";", dec = ",")

M = numeric()
for (i in 1:950) {
  M = rbind(M, rnorm(N, mean = 2, sd = 2))
}
M = as.data.frame(M)
names(M) = names(intervalle_confiance)

intervalle_confiance = rbind(intervalle_confiance, M) # Concat matrices
intervalle_confiance

# 2)

TRUST_THRESHOLD = .95

studentInterval = function (dataFrame, n, trustThreshold) {
  ALPHA = 1 - TRUST_THRESHOLD
  
  foo = apply(dataFrame, 1, function(data) {
    MEAN = mean(data)
    STD_DEV = sqrt(apply(data, 0, var))
    STUDENT_QUANTILE = qt(1 - ALPHA / 2, n - 1)
    ERROR = STUDENT_QUANTILE * STD_DEV / sqrt(n)
    MIN = MEAN - ERROR
    MAX = MEAN + ERROR
    print(sprintf('MIN %f MAX %f', MIN, MAX))
  })
}

studentInterval(intervalle_confiance, N, TRUST_THRESHOLD)
