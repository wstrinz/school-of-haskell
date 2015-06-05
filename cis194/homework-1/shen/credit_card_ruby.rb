class CreditCard
  def to_digits(num)
    if num < 0
      []
    else
      num.to_s.split("").map(&:to_i)
    end
  end

  def to_digits_rev(num)
    to_digits(num).reverse
  end

  def double_every_other(list)
    list.reverse.each_slice(2).map do |(first, last)|
      [first, last ? last * 2 : last]
    end.flatten.compact.reverse
  end

  def sum_digits(list)
    list.map(&method(:to_digits)).flatten.inject(&:+)
  end

  def validate(num)
    sum_digits(double_every_other(to_digits(num))) % 10 == 0
  end
end
