/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.ejb;

import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.servlet.jsp.jstl.core.Config;
import spockdataaccess.entity.Configuration;
import spockdataaccess.entity.Experiment;

/**
 *
 * Responsible for managing high and low level requests to and from the database.
 * 
 * @author Loren Chorley
 */
@Stateful
public class RequestBean {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.RequestBean");
    
    @PersistenceContext
    private EntityManager em;
    
    public void createConfig(String name, String value) {
        
        try {
        
            Configuration config = new Configuration();
            
            config.setName(name);
            config.setConfigValue(value);
            
            em.persist(config);
            
            logger.log(Level.INFO,
                       "Created and persisted configuration: {0}-{1}",
                       new Object[] { name, value });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createExperiment threw: " + ex.getMessage());
        }
        
    }
    
    public String getConfig(String name) {
        String rtnval = null;
        
        try {
            
            Configuration c = (Configuration) em.createNamedQuery("Configuration.findByName")
                                                .setParameter("name", name)
                                                .getSingleResult();
            
            rtnval = c.getConfigValue();
            
        } catch (NoResultException e) {
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createExperiment threw: " + ex.getMessage());
        } finally {
            return rtnval;
        }
        
    }
    
    public void setConfig(String name, String value) {
        //TODO
    }
    
    public void createExperiment(String id) {
        
        try {
        
            Experiment experiment = new Experiment();
            experiment.setId(id);
            experiment.setIsActive(Boolean.FALSE);

            em.persist(experiment);
            
            logger.log(Level.INFO,
                       "Created and persisted experiment: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createExperiment threw: " + ex.getMessage());
        }
        
    }
    
    public void setExperimentActivation(String id, boolean activation) {
        List experiments = em.createNamedQuery("findAllExperiments") //TODO Can be improved to jus return required result
                             .getResultList();
        
        try {
        
            for (Iterator it = experiments.iterator(); it.hasNext();) {
                Experiment experiment = (Experiment) it.next();
                if (experiment.getId().compareTo(id) == 0) {
                    experiment.setIsActive(activation);
                    break;
                }
            }
        
        } catch (Exception ex) {
            throw new EJBException("RequestBean.setExperimentActivation threw: " + ex.getMessage());
        }
        
    }
    
    /*
     * Testing function for deleting all data
     */
    public void cleanDatabase() {
        try {
            
            em.createQuery("DELETE FROM Experiment e")
              .executeUpdate();
            
            em.createQuery("DELETE FROM Configuration c")
              .executeUpdate();
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.cleanDatabase threw: " + ex.getMessage());
        }
    }
    
    
}
