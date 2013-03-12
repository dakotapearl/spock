package spockdataaccess.ejb.requestsupport.generalisations;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;

/**
 * Supplies a generalised set of functions for managing collections of entities within another entity.
 * @author Loren Chorley
 * @param <Econtainer> Entity container - the entity that contains the collection of entities
 * @param <E> Entity collection - the entity collection contained within the container entity
 * @param <ID> the type of the entities id field
 */
public abstract class BasicEntityCollection<Econtainer, E, ID> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.BasicEntityCollection");
    
    protected EntityManager em;
    
    public BasicEntityCollection(EntityManager em) {
        this.em = em;
    }
    
    public E setEntityWithinCollection(Econtainer container, E entity) {
        try {
            
            Collection<E> collection = getCollection(container);
            
            logger.log(Level.INFO,
                           "Getting entity collection id",
                           new Object[] {  });
            
            ID id = getCollectionEntityID(entity);
            E e = null;
            
            if (id != null) {
                logger.log(Level.INFO,
                           "attempting to find entity: {0}",
                           new Object[] { entity.toString() });
                e = (E) em.find(getCollectionEntityClass(), id);
                logger.log(Level.INFO,
                           "'found' entity: {0}",
                           new Object[] { entity.toString() });
            } else {
                logger.log(Level.INFO,
                           "ignoring entity: {0}",
                           new Object[] { entity.toString() });
            }
            
            logger.log(Level.INFO,
                           "between",
                           new Object[] {  });
            
            // If entity not found in persistence context
            if (e == null) {
                
                logger.log(Level.INFO,
                           "creating new entity: {0}",
                           new Object[] { entity.toString() });
                
                // Create new entity object to avoid persistence problems with already already persisted objects
                e = newCollectionEntity(id);
                
                logger.log(Level.INFO,
                           "copying: {0}",
                           new Object[] { entity.toString() });
                
                // copy contents
                copyCollectionEntityProperties(entity, e);
                
                logger.log(Level.INFO,
                           "adding: {0}",
                           new Object[] { entity.toString() });
                
                // add to collection
                collection.add(e);
                
                logger.log(Level.INFO,
                           "persisting: {0}",
                           new Object[] { entity.toString() });
                
                // persist
                em.persist(e);
                
                logger.log(Level.INFO,
                           "Created new collection entity: {0}",
                           new Object[] { entity.toString() });
                
            } else {
                
                logger.log(Level.INFO,
                           "else: copying: {0}",
                           new Object[] { entity.toString() });
                
                // Update the entities properties
                copyCollectionEntityProperties(entity, e);
                
                logger.log(Level.INFO,
                           "before if: {0}",
                           new Object[] { entity.toString() });
                
                // then if the collection does not contain the entity, add the entity to the collection
                if (!collection.contains(entity)) {
                    
                    logger.log(Level.INFO,
                           "not in collection, adding: {0}",
                           new Object[] { entity.toString() });
                    
                    collection.add(e);
                    
                    logger.log(Level.INFO,
                           "added entity: {0}",
                           new Object[] { entity.toString() });
                    
                }
                
            }
            
            logger.log(Level.INFO,
                           "returning entity: {0}",
                           new Object[] { entity.toString() });
            
            return e;
            
        } catch (Exception ex) {
            throw new EJBException("setEntityWithinCollection threw: " + ex.getMessage());
        }
    }
    
    public List<E> retrieveEntityWithinCollection(Econtainer container, ID id) {
        try {
            
            Collection<E> collection = getCollection(container);
            
            // if id not given, return the whole collection as a list
            // This cast is ok since the collections are implemented as array lists
            if (id == null) {
                return (List<E>) collection;
            } else {
                
                E e = (E) em.find(getCollectionEntityClass(), id);
                
                // If e is null, it was not found in the persistence context
                // therefore it cannot be in the collection, so we return null
                if (e == null) {
                    return null;
                } else {
                    
                    // If the collection contains the entity, return it in a list of one
                    if (collection.contains(e)) {
                        List<E> returnList = new ArrayList<E>();
                        returnList.add(e);
                        return returnList;
                    } else { // otherwise return null
                        return null;
                    }
                }
            }
            
        } catch (Exception ex) {
            throw new EJBException("retrieveEntityWithinCollection threw: " + ex.getMessage());
        }
    }
    
    public Integer countEntitiesWithinCollection(Econtainer container) {
        try {
        
            return getCollection(container).size();
        
        } catch (Exception ex) {
            throw new EJBException("countEntitiesWithinCollection threw: " + ex.getMessage());
        }
    }
    
    public void removeEntityFromCollectionByID(Econtainer container, ID id) {
        try {
            
            E e = (E) em.find(getCollectionEntityClass(), id);
            
            if (e != null) {
                removeEntityFromCollection(container, e);
            } else {
                throw new EJBException("Entity with id '" + id + "' not found in collection when attempting to remove!");
            }
            
        } catch (Exception ex) {
            throw new EJBException("removeEntityFromCollectionByID threw: " + ex.getMessage());
        }
    }
    
    public void removeEntityFromCollection(Econtainer container, E entity) {
        try {
            
            throw new EJBException("Not yet implemented!");
            
        } catch (Exception ex) {
            throw new EJBException("removeEntityFromCollection threw: " + ex.getMessage());
        }
    }
    
    protected abstract Collection<E> getCollection(Econtainer container);
    protected abstract E newCollectionEntity(ID id);
    protected abstract void copyCollectionEntityProperties(E sourceEntity, E targetEntity);
    protected abstract ID getCollectionEntityID(E entity);
    protected abstract Class getCollectionEntityClass();
    protected abstract void verifyBusinessLogic(E entity);
    
}
