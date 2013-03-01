package spockdataaccess.entity;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 * This entity can only exist with an environment and an environment interface.
 * 
 * @author Loren Chorley
 */
@Entity
@Table(name = "EnvironmentNodes")
public class EnvironmentNode implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @NotNull
    @ManyToOne
    protected Environment environment;
    @NotNull
    @ManyToOne
    protected EnvironmentInterface environmentInterface;
    
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Environment getEnvironment() {
        return environment;
    }

    public EnvironmentInterface getEnvironmentInterface() {
        return environmentInterface;
    }

    public void setEnvironment(Environment environment) {
        this.environment = environment;
    }

    public void setEnvironmentInterface(EnvironmentInterface environmentInterface) {
        this.environmentInterface = environmentInterface;
    }
    

    
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof EnvironmentNode)) {
            return false;
        }
        EnvironmentNode other = (EnvironmentNode) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.EnvironmentNode[ id=" + id + " ]";
    }
    
}
