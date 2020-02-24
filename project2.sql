use retailer1;
show tables;
select * from hshldDemograph;
select * from itemsAttributes;
select * from randItemSales;
select * from storeItemSales;
select * from survItemSales;

/*1. Describe the percentage of sales of existing flavors in the Greek yogurt category (all brands)*/
/*we calculated the total Greek yogurt sales as 23248899.49*/
select sum(sales) from storeItemSales join itemsAttributes 
on storeItemSales.`Item.Num`=itemsAttributes.`Item.Num` 
where class = 'Greek';

select Flavor1, sum(sales), sum(sales)/23248899.49 as percentage_of_sales from storeItemSales join itemsAttributes 
on storeItemSales.`Item.Num`=itemsAttributes.`Item.Num` 
where class = 'Greek'
and (Flavor1 = 'blueberry' or Flavor1 = 'Honey' or Flavor1 = 'Peach' or Flavor1 = 'Plain' or Flavor1 = 'Strawberry' or Flavor1 = 'Vanilla')
group by Flavor1
order by percentage_of_sales desc;

/*2. Describe the percentage of sales of existing yogurt flavors outside of Greek yogurt (regular
class of yogurt)*/
/*we calculated the total Greek yogurt sales as 68303678.66*/
select sum(sales) from storeItemSales join itemsAttributes 
on storeItemSales.`Item.Num`=itemsAttributes.`Item.Num` 
where class = 'Regular';

select Flavor1, sum(sales), sum(sales)/68303678.66 as percentage_of_sales from storeItemSales join itemsAttributes 
on storeItemSales.`Item.Num`=itemsAttributes.`Item.Num` 
where class = 'Regular'
and (Flavor1 = 'blueberry' or Flavor1 = 'Honey' or Flavor1 = 'Peach' or Flavor1 = 'Plain' or Flavor1 = 'Strawberry' or Flavor1 = 'Vanilla')
group by Flavor1
order by percentage_of_sales desc;

