package week3

object Main extends App {
  val account = new BankAccount
  account.deposit(50)
  account.withdraw(20)
  account.withdraw(20)
  account.withdraw(15)
}