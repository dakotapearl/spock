package spockdataaccess.ejb.requestsupport;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;

/**
 * This class determines the basic functions for an entity, though requires some basic functions to be implemented by the entity management class.
 * 
 * @author Loren Chorley
 */
public abstract class BasicEntityFunctions<E, ID> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.BasicEntityFunctions");
    
    private EntityManager em;
    
    public BasicEntityFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * If no entity exists which can be identeified by the passed entities id, then it creates a new entity with this id and sets its properties to match. Otherwise it just set the properties to match, excluding the id property.
     * @param entity the entity to set or creates
     */
    public E setEntity(E entity) {
        
        try {
            
            ID id = getEntityID(entity);
            
            E e = (E) em.find(getEntityClass(), id);
            
            // If entity not found in persistence context
            if (e == null) {
                
                // Create new entity object to avoid persistence problems with already already persisted objects
                e = newEntity(id);
                
                logger.log(Level.INFO,
                           "Created new entity: {0}",
                           new Object[] { id.toString() });
                
            }
            
            copyEntityProperties(entity, e);
            
            em.persist(e);
            
            logger.log(Level.INFO,
                       "Set entity: {0}",
                       new Object[] { id.toString() });
            
            return e;
            
        } catch (Exception ex) {
            throw new EJBException("BasicEntityFunctions threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * if ID is set, return matching entity, null if entity doesn't exist. If ID is null, return all entities.
     * @param id
     * @return 
     */
    public List<E> retrieveEntity(ID id) {
        
        List<E> returnList = null;
        
        try {
            
            // Get all entities
            if (id == null) {
                
                returnList = (List<E>) em.createQuery("SELECT x FROM " + getEntityName() + " x").getResultList();
                
            } else { // Get just the one entity
                
                E e = (E) em.find(getEntityClass(), id);
                
                if (e != null) {
                    returnList = new ArrayList<E>();
                    returnList.add(e);
                }
                
            }
            
        } catch (Exception ex) {
            throw new EJBException("BasicEntityFunctions threw: " + ex.getMessage());
        }
        
        return returnList;
        
    }
    
    /**
     * if ID is set, return matching entity, null if entity doesn't exist. If ID is null, return all entities.
     * @param id
     * @return 
     */
    public Long countEntities() {
        
        Long count = null;
        
        try {
            
            count = (Long) em.createQuery("SELECT COUNT(x) FROM " + getEntityName() + " x").getSingleResult();
            
        } catch (Exception ex) {
            throw new EJBException("BasicEntityFunctions threw: " + ex.getMessage());
        }
        
        return count;
        
    }
    
    /**
     * 
     * @param id
     */
    public void removeEntity(ID id) {
        
        try {
            
            E e = (E) em.find(getEntityClass(), id);
            
            if (e == null) {
                throw new EJBException("Entity with id '" + id + "' not found when attempting to remove!");
            }
            
            em.remove(e);
            
            logger.log(Level.INFO,
                       "Removed entity: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("BasicEntityFunctions threw: " + ex.getMessage());
        }
        
        
    }
    
    protected abstract E newEntity(ID id);
    protected abstract void copyEntityProperties(E sourceEntity, E targetEntity);
    protected abstract ID getEntityID(E entity);
    protected abstract String getEntityName();
    protected abstract Class getEntityClass();
    
}


