/*
 * Copyright 2012 Oracle and/or its affiliates.
 * All rights reserved.  You may not modify, use,
 * reproduce, or distribute this software except in
 * compliance with  the terms of the License at:
 * http://developers.sun.com/license/berkeley_license.html
 */


package environment.ejb;

import javax.naming.Context;
import javax.ejb.embeddable.EJBContainer;
import java.util.logging.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author ian
 */
public class EnvironmentBeanTest {
    private static final Logger logger = Logger.getLogger("standalone.ejb");
    private Context ctx;
    private EJBContainer ec;

    public EnvironmentBeanTest() {
    }

    @Before
    public void setUp() {
        ec = EJBContainer.createEJBContainer();
        ctx = ec.getContext();
    }

    @After
    public void tearDown() {
        if (ec != null) {
            ec.close();
        }
    }

    /**
     * Test of returnMessage method, of class StandaloneBean.
     */
    @Test
    public void testReturnMessage() throws Exception {
        logger.info("Testing environment.ejb.EnvironmentBean.returnMessage()");

        EnvironmentBean instance = (EnvironmentBean) ctx.lookup("java:global/classes/StandaloneBean");
        String expResult = "Greetings!";
        String result = instance.returnMessage();
        assertEquals(expResult, result);
    }
}

/* Si je voudrai bean.xml
 * private MyEjb myEjbInstance; 

   @BeforeClass 
   public static void setupTest() throws Exception { 
      Map<Object, Object> properties = new HashMap<Object, Object>(); 
      properties.put(EJBContainer.APP_NAME, "myEjbApp"); 
      ejbContainer = javax.ejb.embeddable.EJBContainer.createEJBContainer(properties); 
   } 

   @Before 
   public void beforeTest() throws Exception { 
      myEjbInstance = (MyEjb)ejbContainer.getContext().lookup("java:global/myEjbApp/classes/MyEjb"); 
   } */