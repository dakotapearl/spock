package spock.dataaccess.ejb.support;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.BasicEntity;

/**
 * This class determines the basic functions for an entity, though requires some basic functions to be implemented by the entity management class.
 * 
 * @author Loren Chorley
 */
public abstract class AbstractBasicEntity<E, ID> implements BasicEntity<E, ID> {
    private static final Logger logger = Logger.getLogger("spock.dataaccess.ejb.support.AbstractBasicEntity");
    
    protected EntityManager em;
    
    @Override
    public void setEntityManager(EntityManager em) {
        this.em = em;
    }
    
    @Override
    public E setEntity(E entity) {
        
        try {
            
            ID id = getEntityID(entity);
            E e = null;
            
            if (id != null) {

                e = (E) em.find(getEntityClass(), id);
            }
            
            // If entity not found in persistence context
            if (e == null) {
                
                // Create new entity object to avoid persistence problems with already already persisted objects
                e = newEntity(id);
                
            }
            
            copyEntityProperties(entity, e);
            
            verifyBusinessLogic(e);
            
            em.persist(e);
            
            logger.log(Level.INFO,
                       "Set entity: {0}",
                       new Object[] { entity.toString() });
            
            return e;
            
        } catch (Exception ex) {
            throw new EJBException("AbstractBasicEntity.setEntity threw: " + ex.getMessage());
        }
        
    }
    
    @Override
    public List<E> retrieveEntity(ID id) {
        
        try {
            
            List<E> returnList = null;
            
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
            
            return returnList;
            
        } catch (Exception ex) {
            throw new EJBException("AbstractBasicEntity.retrieveEntity threw: " + ex.getMessage());
        }
        
    }
    
    @Override
    public Long countEntities() {
        
        try {
            
            Long count = (Long) em.createQuery("SELECT COUNT(x) FROM " + getEntityName() + " x").getSingleResult();
            
            return count;
            
        } catch (Exception ex) {
            throw new EJBException("AbstractBasicEntity.countEntities threw: " + ex.getMessage());
        }
        
    }
    
    @Override
    public void removeEntityByID(ID id) {
        try {
            
            E e = (E) em.find(getEntityClass(), id);
            removeEntity(e);
            
        } catch (Exception ex) {
            throw new EJBException("AbstractBasicEntity.removeEntityByID threw: " + ex.getMessage());
        }
    }
    
    @Override
    public void removeEntity(E entity) {
        
        try {
            
            if (entity == null) {
                return; //TODO throw exception
            }
            
            em.remove(entity);
            
            logger.log(Level.INFO,
                       "Removed entity: {0}",
                       new Object[] { entity.toString() });
            
        } catch (Exception ex) {
            throw new EJBException("AbstractBasicEntity.removeEntity threw: " + ex.getMessage());
        }
        
        
    }
    
    protected abstract E newEntity(ID id);
    protected abstract void copyEntityProperties(E sourceEntity, E targetEntity);
    protected abstract ID getEntityID(E entity);
    protected abstract String getEntityName();
    protected abstract Class getEntityClass();
    protected abstract void verifyBusinessLogic(E entity);
    
}


