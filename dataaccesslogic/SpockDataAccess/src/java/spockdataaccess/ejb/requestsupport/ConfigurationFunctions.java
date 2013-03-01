/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Configuration;

/**
 *
 * @author Loren Chorley
 */
public class ConfigurationFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.ConfigurationFunctions");
    
    private EntityManager em;
    
    public ConfigurationFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Sets the configuration name to value. If it doesn't already exist, it creates it first.
     * @param name the name of the configuration
     * @param value the value to initially give the configuration
     */
    public void setConfiguration(String name, String value) {
        
        try {
            
            Configuration config;
            config = em.find(Configuration.class, name);
            
            if (config == null) {
                
                config = new Configuration();
                
                logger.log(Level.INFO,
                           "Create new configuration: {0}",
                           new Object[] { name });
                
            }
            
            config.setName(name);
            config.setConfigValue(value);
            
            em.persist(config);
            
            logger.log(Level.INFO,
                       "Set configuration: {0}-{1}",
                       new Object[] { name, value });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createExperiment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Retrieves the value of a stored configuration from the database.
     * @param name the name of the configuration
     * @return Returns configuration value, or null if it doesn't exist
     */
    public String getConfiguration(String name) {
        String rtnval = null;
        
        try {
            
            Configuration config = em.find(Configuration.class, name);
            
            if (config != null) {
                rtnval = config.getConfigValue();
            }
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createExperiment threw: " + ex.getMessage());
        } finally {
            return rtnval;
        }
        
    }
    
    /**
     * Removes the stored configuration from the database.
     * @param name the name of the configuration
     */
    public void removeConfiguration(String name) {
        try {
            
            Configuration config = em.find(Configuration.class, name);
            em.remove(config);
            
            logger.log(Level.INFO,
                       "Removed configuration: {0}",
                       new Object[] { name });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createExperiment threw: " + ex.getMessage());
        }
    }
    
}
