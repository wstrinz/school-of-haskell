require_relative 'credit_card'

describe CreditCard do
  describe '#to_digits' do
    it "splits an integer into a list of digits" do
      expect(subject.to_digits 1234).to eq [1,2,3,4]
    end

    it "returns an empty list when an Integer <= 0" do
      expect(subject.to_digits(-2)).to eq []
    end

    describe "to_digits_rev" do
      it "splits an integer into a reversed list of digits" do
        expect(subject.to_digits_rev 1234).to eq [4,3,2,1]
      end

      it "returns an empty list when an Integer <= 0" do
        expect(subject.to_digits_rev(-2)).to eq []
      end
    end
  end

  describe "double_every_other" do
    it "doubles every other integer in a list" do
      expect(subject.double_every_other [8,7,6,5]).to eq [16,7,12,5]
      expect(subject.double_every_other [1,2,3]).to eq [1,4,3]
    end
  end


  describe "sum_digits" do
    it "sums all of the digits of each integer in a list" do
      expect(subject.sum_digits [16,7,12,5]).to eq 22
    end
  end

  describe "validate" do

    it "indicates whether a credit card number is valid" do
      expect(subject.validate 4012888888881881).to be
      expect(subject.validate 4012888888881882).not_to be
    end
  end
end
