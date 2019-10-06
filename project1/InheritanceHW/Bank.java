/*
Clarifying Q's
1) How many types of accounts should I implement? 2 - savings and checkings
2) Is the bank a chain/ should all the info be shared among the chain? No
3) Should I worry about the employees and their salaries etc? No

*/

public class Bank{

  enum Status
  {
    OPEN,CLOSED;
  }

  String name;
  ArrayList<Patron> patrons;
  Status s;

  public Bank(String n){
    name = n;
    patrons = new ArrayList<Patron>();
    s = Status.OPEN;
  }

  public void open_Account(int initial, Patron p){

  }

  public void close(){
    s = Status.CLOSED;
  }

  public void open(){
    s = Status.OPEN;
  }
}

public class Patron{

ArrayList<Account> a;
String name;

 public Patron(String n){
   name = n
  a = new ArrayList<Account>();
 }

 public void create_Account(int money, int id){

 }

 public void withdraw(int money){

 }

}

public class Account{

int holding;
String dateCreated;
int id;

 public Account(int money,String d, int i){
   holding = money;
   dateCreated = d
   id = i;
 }

 public void withdrawBank(int money){

 }

 public void deposit(int money){

 }

 public void getSum(){

 }
}

public class Checkings extends Account{

int pin;
const int widthdrawlLimit;

  public class Checkings(int money,int i, int p){
    super.Account(money,i);
    pin = p;
  }

  public void withdrawATM(int money){

  }

  public void resetPin(int p){

  }

}


public class Savings extends Account{

int interest;
const int minBalance;

  public class Savings(int id, int money, int i){
    super.Account(money, id)
    interest = i;
  }

  public void applyInterest(){

  }

  public void getInterest(){

  }

}
