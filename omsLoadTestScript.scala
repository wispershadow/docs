import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.feeder._
import scala.concurrent.duration._
import io.gatling.commons.validation._
import io.gatling.http.check.HttpCheck
import scala.util.Random
import java.util.concurrent.ThreadLocalRandom

class OMSLoadTestSimulation extends Simulation {
	object Config {
		val CONCURRENT_USERS_PER_SECOND = System.getProperty("currentUserPerSecond", "5").toDouble
		val LOAD_TEST_DURATION = System.getProperty("loadTestDuration", "5").toDouble //minutes
		val THINK_TIME_DEFAULT_SECONDS = Integer.getInteger("thinkTime", 5).toInt
		
		val SERVER = System.getProperty("server", "electronics.local")
		val CONTEXT = System.getProperty("context", "/yacceleratorstorefront/electronics/en/")
		val PAYMENTMOCKROOT = System.getProperty("paymentMock", "/acceleratorservices/")
		val HTTP_PORT = Integer.getInteger("httpPort", 80).toInt
		val HTTPS_PORT = Integer.getInteger("httpsPort", 443).toInt
		
		def serverHttpUrl(): String = {"http://" + SERVER + ":" + HTTP_PORT}
		def serverHttpsUrl(): String = {"https://" + SERVER + ":" + HTTPS_PORT}
    
		def serverRootHttpUrl(): String = {return serverHttpUrl() + CONTEXT}
		def serverRootHttpsUrl(): String = {return serverHttpsUrl() + CONTEXT}
		def paymentMockRootUrl(): String = {return serverHttpsUrl() + PAYMENTMOCKROOT}
		
		val HTML_HEADER = Map("""Accept""" -> """text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8""",
                          """Cache-Control""" -> """max-age=0""")
		
		val AJAX_POST_FORM_HEADER = Map("""Cache-Control""" -> """max-age=0""",
                                    """Content-Type""" -> """application/x-www-form-urlencoded; charset=UTF-8""",
                                    """Pragma""" -> """no-cache""",
                                    """X-Requested-With""" -> """XMLHttpRequest""")
		
		val myBuilder = new StringBuilder();
		def appendStringBuilder(param: String): StringBuilder = {
			if (myBuilder.length > 0) {
				myBuilder.append(",");
			}
			myBuilder.append(param); 
			return myBuilder
		}
		val allLocalAddrs = csv("localaddress.csv").convert{case("localaddr",attrval) => appendStringBuilder(attrval)}
		println(myBuilder.toString())
		val HTTP_PROTOCOL = http.localAddresses("127.0.1.1","127.0.2.1","127.0.3.1").userAgentHeader("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36")
	}
	
	object FeedUtil {
		val addressGrouped: Map[String, IndexedSeq[Record[String]]] = csv("addresses.csv").records.groupBy{ record => record("email") }
		val addressesByCust: Map[String, IndexedSeq[Record[String]]] = addressGrouped.mapValues{ records => records.map {record => record} }
		val cardGrouped: Map[String, IndexedSeq[Record[String]]] = csv("cards.csv").records.groupBy{ record => record("email") }
		val cardsByCust: Map[String, IndexedSeq[Record[String]]] = cardGrouped.mapValues{ records => records.map {record => record} }
		
	}
	
	object PageRequest {
		def selectCSRFToken(): HttpCheck = {
			css("input[name='CSRFToken']", "value").find.saveAs("CSRFToken")
		}
		
		 def selectLoginSuccess(): HttpCheck = {
			css("div#test_header_LoggedUser_\\$1").find.saveAs("loginSuccess")
		}
		
		def selectValidProd(): HttpCheck = {
			css("form#addToCartForm button#addToCartButton", "type").exists
			css("input[name='productCodePost']", "value").find.saveAs("selectedProdCode")
		}
		
		def expCheckoutDetect(): HttpCheck = {
			css("input[class='express-checkout-checkbox']").find.saveAs("expCheckoutEnabled")
		}
		
		def checkoutDetect(): HttpCheck = {
			css("button[class~='checkoutButton']").find.saveAs("checkoutEnabled") 
		}
		
		
		val expCheckoutEnabled = (session: Session) => {
			Success(session.contains("expCheckoutEnabled"))
		}
		
		val checkoutEnabled = (session: Session) => {
			Success(session.contains("checkoutEnabled"))
		}
		
		def selectShippingAddr(): HttpCheck = {
			css("form#addressForm", "action").find.saveAs("loadedAddressForm")
		}
		
		def selectDeliveryMethod(): HttpCheck = {
			status.is(302)
			currentLocationRegex(Config.serverRootHttpsUrl + ".*checkout/multi/delivery-method/choose")
			css("form#selectDeliveryMethodForm", "action").find.saveAs("loadedDeliveryMethod") 
		}
		
		def selectPaymentMethod(): HttpCheck = {
			status.is(302)
			currentLocationRegex(Config.serverRootHttpsUrl + ".*checkout/multi/payment-method/add")
			css("form#silentOrderPostForm", "action").find.saveAs("loadedPaymentMethod") 
		}
		
		def checkPaymentSubmit(): HttpCheck = {
			status.is(200)
		}
		
		def checkSopResponsePost(): HttpCheck = {
			status.is(302)
			currentLocationRegex(Config.serverRootHttpsUrl + ".*checkout/multi/summary/view")
			css("form#placeOrderForm1", "action").find.saveAs("loadedPlaceOrder") 
		}
		
		
		def finalReview(): HttpCheck = {
			css("input[name='termsCheck']").exists
		}
		
		def orderSuccess(): HttpCheck = {
			css("div[class='checkout-success-body'] p b").find.saveAs("orderNumber")
		}
	}
	
	object Login {
		val feeder = csv("user.csv").queue
		val login  = exec(http("Login Page").get(Config.serverRootHttpsUrl + "login").headers(Config.HTML_HEADER).check(PageRequest.selectCSRFToken))
					.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
					.feed(feeder)
					.exec(http("Perform Login").post(Config.serverRootHttpsUrl + "j_spring_security_check").formParam("j_username", "${email}").formParam("j_password", "${pwd}").formParam("CSRFToken", "${CSRFToken}"))
					.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
					.exec(http("Login Check").get(Config.serverRootHttpsUrl()).headers(Config.HTML_HEADER).check(PageRequest.selectLoginSuccess))
					.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
					
	}


    object ProdDtl {
		val feeder = csv("cartprod.csv").random
        var dispProds = feed(feeder)
                 		.exec(session => session.remove("CSRFToken"))
                        .exec(http("Prod Detail Page").get(Config.serverRootHttpsUrl + "p/${product_code}").headers(Config.HTML_HEADER).check(PageRequest.selectValidProd).check(PageRequest.selectCSRFToken))

    }
 
	
	
	object AddToCart {
		val feeder = csv("cartprod.csv").random 
		var addToCart = feed(feeder)
						.exec(session => session.remove("CSRFToken"))
						.exec(http("Prod Detail Page").get(Config.serverRootHttpsUrl + "p/${product_code}").headers(Config.HTML_HEADER).check(PageRequest.selectValidProd).check(PageRequest.selectCSRFToken)) 
						.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
						.doIf(session => (session.contains("selectedProdCode") && session.contains("CSRFToken"))) {
							exec(http("Add to Cart").post(Config.serverRootHttpsUrl + "cart/add").headers(Config.HTML_HEADER).formParam("productCodePost", "${product_code}").formParam("pdpAddtoCartInput", 1).formParam("CSRFToken", "${CSRFToken}"))
							.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
						}	
	}
	
	object Checkout {
		var loadAddressData = exec{session => session("email").validate[String].map{ email => 
																						val addresses = FeedUtil.addressesByCust(email)
																						val selectedAddr = addresses(ThreadLocalRandom.current.nextInt(addresses.length))
																						session.set("selectedAddr", selectedAddr)
																				   }
								  }
		
		var loadCardData = exec{session => session("email").validate[String].map{ email => 
																						val cards = FeedUtil.cardsByCust(email)
																						val selectedCard = cards(ThreadLocalRandom.current.nextInt(cards.length))
																						session.set("selectedCard", selectedCard)
																				}
							   }
		
		var startCheckout = exec(http("Cart Page").get(Config.serverRootHttpsUrl + "cart").headers(Config.HTML_HEADER).check(PageRequest.selectCSRFToken).check(PageRequest.checkoutDetect))
							.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
							.doIf("${checkoutEnabled.exists()}"){
								exec(session => session.remove("CSRFToken"))
								.exec(http("Checkout Page").get(Config.serverRootHttpsUrl + "cart/checkout").headers(Config.HTML_HEADER).check(PageRequest.selectCSRFToken).check(PageRequest.selectShippingAddr))
								.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
								.exec(http("Change Address").get(Config.serverRootHttpsUrl + "my-account/addressform").headers(Config.HTML_HEADER).queryParam("addressCode", "").queryParam("countryIsoCode", "DE").check(PageRequest.selectShippingAddr))
								.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
							}
							
		var startExpCheckout = exec(http("Cart Page").get(Config.serverRootHttpsUrl + "cart").headers(Config.HTML_HEADER).check(PageRequest.selectCSRFToken).check(PageRequest.expCheckoutDetect))					
		
		var postShippingAddr = doIf("${loadedAddressForm.exists()}") {
								exec(http("Post shipping address").post(Config.serverRootHttpsUrl + "checkout/multi/delivery-address/add").headers(Config.AJAX_POST_FORM_HEADER)
								.formParam("CSRFToken", "${CSRFToken}")
								.formParam("_saveInAddressBook", "on")
								.formParam("addressId", "")
								.formParam("bill_state", "")
								.formParam("countryIso", "DE")
								.formParam("townCity", "${selectedAddr.city}")
								.formParam("line1", "${selectedAddr.line1}")
								.formParam("line2", "${selectedAddr.line2}")
								.formParam("phone", "${selectedAddr.phone}")
								.formParam("postcode", "${selectedAddr.postcode}")
								.formParam("titleCode", "${selectedAddr.title}")
								.formParam("firstName", "${selectedAddr.firstName}")
								.formParam("lastName", "${selectedAddr.lastName}")
								.check(PageRequest.selectDeliveryMethod))	
								.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
							  }  
		
		var selDeliveryMethod = doIf("${loadedDeliveryMethod.exists()}") {
									exec(session => session.remove("CSRFToken"))
									.exec(http("Select delivery method").get(Config.serverRootHttpsUrl + "checkout/multi/delivery-method/select").headers(Config.HTML_HEADER).queryParam("delivery_method", "stardard-gross").check(PageRequest.selectPaymentMethod).check(PageRequest.selectCSRFToken)) 
								}

		
		
		var postPaymentMethod = doIf("${loadedPaymentMethod.exists()}") {
								exec(http("Post payment method").post(Config.paymentMockRootUrl + "sop-mock/process").headers(Config.AJAX_POST_FORM_HEADER)
								.formParam("CSRFToken", "${CSRFToken}")
								.formParam("card_cardType", "${selectedCard.cardtype}")
								.formParam("card_nameOnCard", "${selectedCard.nameoncart}")
								.formParam("card_accountNumber", "${selectedCard.cardnumber}")
								.formParam("card_expirationMonth", "${selectedCard.cardexpiremonth}")
								.formParam("card_expirationYear", "${selectedCard.cardexpireyear}")
								.formParam("card_cvNumber", "${selectedCard.cardcvnumber}")
								.formParam("useDeliveryAddress", "true")
								.formParam("billTo_country", "DE")
								.formParam("billTo_titleCode",  "${selectedAddr.title}")
								.formParam("billTo_firstName", "${selectedAddr.firstName}")
								.formParam("billTo_lastName", "${selectedAddr.lastName}")
								.formParam("billTo_street1", "${selectedAddr.line1}")
								.formParam("billTo_street2", "${selectedAddr.line2}")
								.formParam("billTo_city", "${selectedAddr.city}")
								.formParam("billTo_postalCode", "${selectedAddr.postcode}")
								.check(PageRequest.checkPaymentSubmit)
								.check(
									css("form#silentOrderPostForm input#billTo_country", "value").find.optional.saveAs("billTo_country"),
									css("form#silentOrderPostForm input#billTo_city", "value").find.optional.saveAs("billTo_city"),
									css("form#silentOrderPostForm input#billTo_titleCode", "value").find.optional.saveAs("billTo_titleCode"),
									css("form#silentOrderPostForm input#billTo_firstName", "value").find.optional.saveAs("billTo_firstName"),
									css("form#silentOrderPostForm input#billTo_lastName", "value").find.optional.saveAs("billTo_lastName"),
									css("form#silentOrderPostForm input#billTo_street1", "value").find.optional.saveAs("billTo_street1"),
									css("form#silentOrderPostForm input#billTo_street2", "value").find.optional.saveAs("billTo_street2"),
									css("form#silentOrderPostForm input#billTo_postalCode", "value").find.optional.saveAs("billTo_postalCode"),
									css("form#silentOrderPostForm input#card_nameOnCard", "value").find.optional.saveAs("card_nameOnCard"),
									css("form#silentOrderPostForm input#card_cardType", "value").find.optional.saveAs("card_cardType"),
									css("form#silentOrderPostForm input#card_accountNumber", "value").find.optional.saveAs("card_accountNumber"),
									css("form#silentOrderPostForm input#card_expirationYear", "value").find.optional.saveAs("card_expirationYear"),
									css("form#silentOrderPostForm input#card_expirationMonth", "value").find.optional.saveAs("card_expirationMonth"),
									css("form#silentOrderPostForm input#useDeliveryAddress", "value").find.optional.saveAs("useDeliveryAddress"),
									css("form#silentOrderPostForm input#decision", "value").find.optional.saveAs("decision"),
									css("form#silentOrderPostForm input#reasonCode", "value").find.optional.saveAs("reasonCode"),
									css("form#silentOrderPostForm input#CSRFToken", "value").find.optional.saveAs("CSRFToken"),
									css("form#silentOrderPostForm input#ccAuthReply_cvCode", "value").find.optional.saveAs("ccAuthReply_cvCode"),
									css("form#silentOrderPostForm input#paySubscriptionCreateReply_subscriptionIDPublicSignature", "value").find.optional.saveAs("paySubscriptionCreateReply_subscriptionIDPublicSignature"),
									css("form#silentOrderPostForm input#paySubscriptionCreateReply_subscriptionID", "value").find.optional.saveAs("paySubscriptionCreateReply_subscriptionID"),
									css("form#silentOrderPostForm input#decision_publicSignature", "value").find.optional.saveAs("decision_publicSignature")
								))
								.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
								}		
								
		var postSopResp = doIfEquals("${decision}", "ACCEPT") {
						  exec(http("Post sop response").post(Config.serverRootHttpsUrl + "checkout/multi/sop/response").headers(Config.AJAX_POST_FORM_HEADER)		
						  .formParam("CSRFToken", "${CSRFToken}")
						  .formParam("billTo_country", "${billTo_country}")
						  .formParam("billTo_city", "${billTo_city}")
						  .formParam("billTo_titleCode", "${billTo_titleCode}")
						  .formParam("billTo_firstName", "${billTo_firstName}")
						  .formParam("billTo_lastName", "${billTo_lastName}")
						  .formParam("billTo_street1", "${billTo_street1}")
						  .formParam("billTo_street2", "${billTo_street2}")
						  .formParam("billTo_postalCode", "${billTo_postalCode}")
						  .formParam("card_nameOnCard", "${card_nameOnCard}")
						  .formParam("card_cardType", "${card_cardType}")
						  .formParam("card_accountNumber", "${card_accountNumber}")
						  .formParam("card_expirationYear", "${card_expirationYear}")
						  .formParam("card_expirationMonth", "${card_expirationMonth}")
						  .formParam("useDeliveryAddress", "${useDeliveryAddress}")
						  .formParam("decision", "${decision}")
						  .formParam("reasonCode", "${reasonCode}")
						  .formParam("ccAuthReply_cvCode", "${ccAuthReply_cvCode}")
						  .formParam("paySubscriptionCreateReply_subscriptionIDPublicSignature", "${paySubscriptionCreateReply_subscriptionIDPublicSignature}")
						  .formParam("paySubscriptionCreateReply_subscriptionID", "${paySubscriptionCreateReply_subscriptionID}")
						  .formParam("decision_publicSignature", "${decision_publicSignature}")
						  .formParam("savePaymentInfo", "true")
						  .check(PageRequest.selectCSRFToken).check(PageRequest.checkSopResponsePost))
						  .pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
						  }
						  
						  	
		
		var expressCheckout = doIf(PageRequest.expCheckoutEnabled) {
								  exec(http("Express checkout").get(Config.serverRootHttpsUrl + "checkout/multi/express").headers(Config.HTML_HEADER).check(PageRequest.selectCSRFToken))
								  .pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
							  }	
								
		var confirmPlaceOrder = doIf("${loadedPlaceOrder.exists()}") {
									exec(http("Confirm Order").post(Config.serverRootHttpsUrl + "checkout/multi/summary/placeOrder").headers(Config.AJAX_POST_FORM_HEADER).formParam("_termsCheck", "on").formParam("termsCheck", "true").formParam("CSRFToken", "${CSRFToken}").check(PageRequest.orderSuccess))	
									.pause(Config.THINK_TIME_DEFAULT_SECONDS - 1, Config.THINK_TIME_DEFAULT_SECONDS + 1)
								}
	}
	

	/**
	val scn = scenario("OMS Load Test Scenario")
			  .exec(Login.login)
			  .doIf("${loginSuccess.exists()}") {
				repeat(3) {exec(AddToCart.addToCart)}
				.exec(Checkout.startExpCheckout)
				.exec(Checkout.expressCheckout)
				.exec(Checkout.confirmPlaceOrder)
			  }
	*/

    /*
	exec { session =>
		println(session)
		session
	}
	*/

       
        val scn = scenario("OMS Load Test Scenario")
                          .exec(Login.login)
                          .doIf("${loginSuccess.exists()}") {
                                exec(ProdDtl.dispProds)
                          }
        

	
	/*	
	val scn = scenario("OMS Load Test Scenario")
			  .exec(Login.login)
			  .doIf("${loginSuccess.exists()}") {
				repeat(3) {exec(AddToCart.addToCart)}
				.exec(Checkout.loadAddressData)
				.exec(Checkout.loadCardData)
				.exec(Checkout.startCheckout)
				.exec(Checkout.postShippingAddr)
				.exec(Checkout.selDeliveryMethod)
				.exec(Checkout.postPaymentMethod)
				.exec(Checkout.postSopResp)
				.exec(Checkout.confirmPlaceOrder)
			  }	 	
	*/		  
	
	setUp(
		
			scn.inject(
			/* atOnceUsers(1) */
			    constantUsersPerSec(Config.CONCURRENT_USERS_PER_SECOND) during(Config.LOAD_TEST_DURATION minutes)
			).protocols(Config.HTTP_PROTOCOL)
		
		
	).throttle(reachRps(50) in (10 seconds), holdFor(2 minute), jumpToRps(200), holdFor(30 minute))
}
