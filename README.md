<h1>Precision Modeling for Midas Automotive Shop</h1>

Background: Patrick owns five Midas automotive repair shops. To keep track of each shop, he visits one shop every day. He suspects that each shop is more productive when he is there as opposed to when he is not. He wants to make a data-driven decision about his visit schedule to each of the five shops.

carsDF has 10 weeks’ worth data:
<li>observation: Unique identifier for an observation. In total, there are 250 observations representing the previous 10 weeks of business (5 shops ~ 10 weeks ~ 5 days week = 250 observations).</li>
<li>shopID: A unique shop identifier for each of the five shops.</li>
<li>boss: A binary variable equal to one when the boss worked at the given store on that day. Note: The boss did not visit each shop equally.</li>
<li>carsFixed: This represents the number of cars that the shop fixed on that given workday.</li>
