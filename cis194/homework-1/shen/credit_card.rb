require 'shen_ruby'
class CreditCard
  def self.shen
    if @shen
      @shen
    else
      @shen = ShenRuby::Shen.new

      puts @shen.eval_string <<-SHEN
(defmacro pipe
    [~> X F] -> [F X]
    [~> X F | Fs] -> [~> [F X] | Fs])

      SHEN
      puts @shen.eval_string("(tc +)")
      puts load_shen_files
      @shen
    end
  end

  def self.load_shen_files
    @shen.eval_string ('(load "credit_card.shen")')
  end

  def initialize
    @shen = self.class.shen
  end

  def to_digits(num)
    ShenRuby.list_to_array(@shen.to_digits(num))
  end

  def to_digits_rev(num)
    ShenRuby.list_to_array(@shen.to_digits_rev(num))
  end

  def double_every_other(list)
    ShenRuby.list_to_array(@shen.double_every_other(ShenRuby.array_to_list(list)))
  end

  def sum_digits(list)
    @shen.sum_digits(ShenRuby.array_to_list(list))
  end

  def validate(num)
    @shen.validate(num)
  end
end
