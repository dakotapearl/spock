package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Metric;

/**
 *
 * @author Loren Chorley
 */
public class MetricFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.MetricFunctions");
    
    private EntityManager em;
    
    public MetricFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Set a value in or create a new network metric
     * @param name the ID of the network metric
     * @param codeURL the location of the binary classes for the network metric
     */
    public void setMetric(String name, String codeURL) {
        
        try {
            
            Metric metric = em.find(Metric.class, name);
            
            if (metric == null) {
                
                metric = new Metric();
                
                logger.log(Level.INFO,
                           "Created new network metric: {0}",
                           new Object[] { name });
                
            }
            
            metric.setId(name);
            metric.setCodeURL(codeURL);
            
            em.persist(metric);
            
            logger.log(Level.INFO,
                       "Set network metric: {0}-{1}",
                       new Object[] { name, codeURL });
            
        } catch (Exception ex) {
            throw new EJBException("MetricFunctions.addMetric threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Retrieve the Code RUL for a network metric
     * @param name the ID of the network metric
     * @return the code URL
     */
    public String getMetricCodeURL(String name) {
        String rtnval = null;
        
        try {
            
            Metric metric = em.find(Metric.class, name);
            rtnval = metric.getCodeURL();
            
        } catch (Exception ex) {
            throw new EJBException("MetricFunctions.getMetric threw: " + ex.getMessage());
        } finally {
            return rtnval;
        }
        
    }
    
    /**
     * Remove the network metric from the database
     * @param name the ID of the network metric
     */
    public void removeMetric(String name) {
        try {
            
            Metric metric = em.find(Metric.class, name);
            em.remove(metric);
            
            logger.log(Level.INFO,
                       "Removed network metric: {0}",
                       new Object[] { name });
            
        } catch (Exception ex) {
            throw new EJBException("MetricFunctions.removeMetric threw: " + ex.getMessage());
        }
    }
    
}
