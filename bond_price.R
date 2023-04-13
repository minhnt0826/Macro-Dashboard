# price_bond <- function(coupon, par, current_rate, duration){
#   current_rate = current_rate / 100
#   
#   price_a = (coupon * (1 - (1 + current_rate) ^(-duration))) / current_rate
#   price_b = par / ((1 + current_rate) ^ duration)
#   
#   price = price_a + price_b
#   return(price_b)
# }

print(price_bond(5, 100, 1, 2))
# print(price_bond(11, 100, 16, 10))
